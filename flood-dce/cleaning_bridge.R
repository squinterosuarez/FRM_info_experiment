## cleaning_bridge.R — Qualtrics pilot export -> apollo-ready long database.
## Maps treatment_arm->treatment, task*_choice->choice (1=A/2=B/3=SQ), T2->prior-gap
## category, and merges each respondent's (block, choice_set) with the locked design
## to carry the effects-coded A_*/B_* attribute columns. Writes data/pilot_database.rds.
suppressMessages({ source("R/00_config.R"); source("R/01_design.R") })

## 1. recover choice_set for each (block, task) in design_wide (read_locked_design
##    dropped it; reconstruct with the identical block/choice_set ordering).
comp <- read.csv(CFG$design_path, stringsAsFactors = FALSE)
ids  <- unique(comp[, c("choice_set","block")])
ids  <- ids[order(ids$block, ids$choice_set), ]
ids$task <- ave(ids$choice_set, ids$block, FUN = seq_along)
dw   <- merge(design_wide, ids, by = c("block","task"))
eff  <- grep("^[AB]_(a1e[0-9]|a3e[0-9]|a4e1|cost100)$", names(dw), value = TRUE)
stopifnot(length(eff) == 14)                       # 7 A_ + 7 B_ effects columns
keymap <- dw[, c("block","choice_set", eff)]

## 2. read pilot. The machine-name header row was removed upstream, so the
##    surprise (T2) and stated-prior (K2) items are keyed by question text;
##    locate them by pattern. ResponseId is gone -> use row index as ID.
d <- read.csv("data/pilot_results.csv", stringsAsFactors = FALSE, check.names = FALSE)
t2col <- grep("different from what you had expected", names(d), ignore.case = TRUE)
k2col <- grep("Environment Agency classifies",       names(d), ignore.case = TRUE)
stopifnot(length(t2col) == 1, length(k2col) == 1)
d$T2 <- d[[t2col]]; d$K2 <- d[[k2col]]
d <- d[d$treatment_arm %in% c("0","1"), ]      # safety: keep assigned-arm respondents
d$ID <- seq_len(nrow(d))

## 3. prior-gap category from T2 surprise item (correct = all dummies 0)
d$catUnder  <- as.integer(grepl("higher",  d$T2))
d$catOver   <- as.integer(grepl("lower",   d$T2))
d$catDK     <- as.integer(grepl("clear",   d$T2))
d$noPrior   <- as.integer(grepl("know",    d$K2, ignore.case = TRUE))  # K2 "I don't know"
d$treatment <- as.integer(d$treatment_arm)
d$catlab <- ifelse(d$catUnder==1,"Under", ifelse(d$catOver==1,"Over",
            ifelse(d$catDK==1,"DK","Correct")))

## 4. wide -> long (one row per respondent x task)
ch_map <- c(A=1L, B=2L, SQ=3L)
long <- do.call(rbind, lapply(seq_len(nrow(d)), function(i) {
  r  <- d[i, ]
  cs <- as.integer(unlist(r[paste0("task",1:6,"_cs")]))
  co <- ch_map[as.character(unlist(r[paste0("task",1:6,"_choice")]))]
  data.frame(ID=r$ID, task=1:6, block=as.integer(r$DCE_Block), choice_set=cs,
             choice=as.integer(co), treatment=r$treatment,
             catUnder=r$catUnder, catOver=r$catOver, catDK=r$catDK,
             noPrior=r$noPrior, catlab=r$catlab, stringsAsFactors=FALSE)
}))

## 5. attach effects columns by (block, choice_set); availability all = 1
db <- merge(long, keymap, by = c("block","choice_set"), all.x = TRUE)
db$av_A <- 1L; db$av_B <- 1L; db$av_SQ <- 1L
db <- db[order(db$ID, db$task), ]; rownames(db) <- NULL

## 6. sanity
stopifnot(!anyNA(db[, eff]))          # every task matched a design row
stopifnot(all(db$choice %in% 1:3))
stopifnot(nrow(db) == nrow(d) * 6)
saveRDS(db, "data/pilot_database.rds")

cat(sprintf("wrote data/pilot_database.rds: %d obs, %d respondents\n", nrow(db), length(unique(db$ID))))
cat("choice (1=A,2=B,3=SQ):\n"); print(table(db$choice))
cat("respondent-level category:\n"); print(table(d$catlab))
cat("arm x category (respondents):\n"); print(table(arm=d$treatment, cat=d$catlab))
