struct pos {
1: i32 x
2: i32 y
}



typedef map<i32,map<i32,map<pos,double>>> matrix3




service TF  {
   double step(1:matrix3 emb),
}

