"""sim_check.py -- mirror of the R DGP (directional + no-idea-via-imputed-gap).
R is authoritative; identical true params. Produces cell sizes + model-free
sanity check, since R/CRAN is unavailable in this environment."""
import numpy as np, pandas as pd
rng=np.random.default_rng(20260528); N=1500; ANCHOR=2.5
mu=dict(asc=-0.40,a1e1=0.50,a1e2=0.00,a1e3=0.40,a2e1=0.30,a3e1=-0.20,a3e2=0.40,a4e1=0.50,cost=-0.80)
dt=dict(asc=-0.05,**{k:0 for k in mu if k!='asc'})
up=dict(asc=-0.15,a1e1=-0.10,a1e2=0.20,a1e3=0.00,a2e1=0.15,a3e1=0.20,a3e2=0.10,a4e1=0.15,cost=0)
dn=dict(asc=0.20,a1e1=-0.20,a1e2=-0.05,a1e3=0.00,a2e1=-0.15,a3e1=-0.15,a3e2=-0.10,a4e1=0.00,cost=0)
NP_ASC=-0.10
sd=dict(asc=1.0,a1e1=0.5,a1e2=0.5,a1e3=0.4,a2e1=0.4,a3e1=0.4,a3e2=0.4,a4e1=0.4)

actual=rng.choice([4,3,2,1],N,p=[0.30,0.22,0.25,0.23]); treatment=rng.integers(0,2,N)
no_idea=rng.random(N)<0.25; shift=rng.choice([-1,0,1,2],N,p=[0.10,0.35,0.35,0.20])
prior=np.clip(actual-shift,1,4)
prior_used=np.where(no_idea,ANCHOR,prior); gap=actual-prior_used
gapUp=np.clip(gap,0,None); gapDown=np.clip(-gap,0,None); noPrior=no_idea.astype(int)
upd=np.where(treatment==0,'control',np.where(gap>0,'upward',np.where(gap<0,'downward','none')))

def alt():
    return dict(a1=rng.integers(1,5,24),a2=rng.integers(1,3,24),a3=rng.integers(1,4,24),
                a4=rng.choice([1,3],24),cost=rng.choice([75,150,300],24))
A,B=alt(),alt(); block=rng.permutation(np.tile(np.arange(4),N//4+1)[:N])
tasks={b:np.where(np.arange(24)//6==b)[0] for b in range(4)}
def a1pw(l,b):return b['a1e1'] if l==1 else b['a1e2'] if l==2 else b['a1e3'] if l==3 else -(b['a1e1']+b['a1e2']+b['a1e3'])
def a2pw(l,b):return b['a2e1'] if l==1 else -b['a2e1']
def a3pw(l,b):return b['a3e1'] if l==1 else b['a3e2'] if l==2 else -(b['a3e1']+b['a3e2'])
def a4pw(l,b):return b['a4e1'] if l==1 else -b['a4e1']
def V(d,k,b):return a1pw(d['a1'][k],b)+a2pw(d['a2'][k],b)+a3pw(d['a3'][k],b)+a4pw(d['a4'][k],b)+b['cost']*d['cost'][k]/100
def beta(i):
    b={}
    for k in mu:
        v=mu[k]+treatment[i]*(dt[k]+up[k]*gapUp[i]+dn[k]*gapDown[i])
        if k=='asc': v+=treatment[i]*noPrior[i]*NP_ASC
        if k in sd: v+=sd[k]*rng.standard_normal()
        b[k]=v
    return b
rows=[]
for i in range(N):
    b=beta(i)
    for k in tasks[block[i]]:
        g=lambda:-np.log(-np.log(rng.random()))
        U=[V(A,k,b)+g(),V(B,k,b)+g(),b['asc']+g()]; ch=int(np.argmax(U))
        rows.append((i,treatment[i],upd[i],noPrior[i],ch,
                     A['a1'][k],A['a2'][k],A['a3'][k],B['a1'][k],B['a2'][k],B['a3'][k]))
df=pd.DataFrame(rows,columns=['ID','t','upd','nop','ch','Aa1','Aa2','Aa3','Ba1','Ba2','Ba3'])

print("="*64,"\nDIRECTIONAL POOLS (treated)\n","="*64,sep="")
tr=treatment==1
print(pd.crosstab(pd.Series(upd)[tr], pd.Series(np.where(no_idea,'no-idea','stated'))[tr], margins=True))
print("\n"+"="*64,"\nRAW SANITY (chosen non-SQ shares, by effective updater)\n","="*64,sep="")
for c,A_,B_ in [('a1','Aa1','Ba1'),('a2','Aa2','Ba2'),('a3','Aa3','Ba3')]:
    df['p_'+c]=np.where(df.ch==0,df[A_],np.where(df.ch==1,df[B_],np.nan))
df['sq']=(df.ch==2).astype(int)
def row(g):
    s=df[df.upd==g]; n=s[s.sq==0]
    return pd.Series({'n_resp':int((pd.Series(upd)==g).sum()),'SQ':s.sq.mean(),
        'A1_highrisk':(n.p_a1==2).mean(),'A1_optin':(n.p_a1==4).mean(),
        'A2_national':(n.p_a2==1).mean(),'A3_riskpay':(n.p_a3==3).mean()})
pd.set_option('display.width',200,'display.float_format',lambda x:f'{x:.3f}')
print(pd.DataFrame({g:row(g) for g in ['control','none','upward','downward']}).T.to_string())
