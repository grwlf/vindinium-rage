
typedef map<i32,map<i32,map<i32,double>>> matrix3

service TF  {
   double step(1:matrix3 emb),
}

