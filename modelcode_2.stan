data{
  int N;                     //# Number of people
  int P;                     //# Number of parameters
  int S;                     //# Number of sites
  int Q;                     //# Number of questions
  int D;                     //# Number of deities
  int K;                     //# Number of responce options

  int Site[N];               //# Site of observation

  vector[N] Age;
  vector[N] Male;
  vector[N] Education;

  int<lower=1,upper=9> OutcomeMoralistic[N,Q];
  int<lower=1,upper=9> OutcomeLocal[N,Q];
}

transformed data{
 int Outcome[N,Q,D];
 int Bleeding[N,Q,D];
 real Delta[N,Q,D];

 for (q in 1:Q){    
 for (i in 1:N){ 
  Outcome[i,q,1] = OutcomeMoralistic[i,q];
  Outcome[i,q,2] = OutcomeLocal[i,q];
  }}

 for (q in 1:Q){    
 for (i in 1:N){ 
  Bleeding[i,q,1] = OutcomeMoralistic[i,q];
  Bleeding[i,q,2] = OutcomeMoralistic[i,q];
  }}

 for (q in 1:Q){    
 for (i in 1:N){   
  Delta[i,q,1] = 0;
  Delta[i,q,2] = 1;
               }}
}

parameters{
  vector[P] MuM;                 //# Cross-population means for each question
  vector<lower=0>[P] SigmaM;     //# Cross-population dispersion
  cholesky_factor_corr[P] LM;    //# Correlations
  
  vector[P] MuL;                 //# Cross-population means for each question
  vector<lower=0>[P] SigmaL;     //# Cross-population dispersion
  cholesky_factor_corr[P] LL;    //# Correlations

  vector[P] Alpha[S,Q,D];          
  ordered[K-1] C;

  simplex[K] Shape[S];
}

transformed parameters {
vector[P] Beta[S,Q,D];
vector[K] C_Shape[S];

for(s in 1:S){
for(q in 1:Q){
 Beta[s,q,1] = MuM + SigmaM .* (LM * Alpha[s,q,1]);
 Beta[s,q,2] = MuL + SigmaL .* (LL * Alpha[s,q,2]);
}}

for(s in 1:S){
 C_Shape[s] = cumulative_sum(Shape[s]);
 }

}

model{
real Xi;
//########################################### Priors
  MuM ~ normal(0,5);
  SigmaM ~ cauchy(0,2.5);
  LM ~ lkj_corr_cholesky(2.5);
  
  MuL ~ normal(0,5);
  SigmaL ~ cauchy(0,2.5);
  LL ~ lkj_corr_cholesky(2.5);

  for(s in 1:S){
for(q in 1:Q){
for(d in 1:D){
  Alpha[s,q,d] ~ normal(0, 1);
 }}}        

 for(s in 1:S){
  Shape[s] ~ dirichlet(rep_vector(1,K));
 }      

//########################################## Model outcomes
 for (d in 1:D){     //# For each deity
  for (q in 1:Q){    //# For each question
   for (i in 1:N){   //# Loop over individuals 
     if(Outcome[i,q,d]<8 && Bleeding[i,q,d]<8){ //# Check if response is missings
     
     //# Define model
       Xi = Beta[Site[i],q,d,1] + Beta[Site[i],q,d,2]*C_Shape[Site[i],Bleeding[i,q,d]]*Delta[i,q,d];
    
     //# Model outcomes
       Outcome[i,q,d] ~ ordered_logistic(Xi, C);
                      }
       }}}

}

