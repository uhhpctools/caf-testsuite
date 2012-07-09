/* Copyright 2014 The University of Edinburgh */

/* Licensed under the Apache License, Version 2.0 (the "License"); */
/* you may not use this file except in compliance with the License. */
/* You may obtain a copy of the License at */

/*     http://www.apache.org/licenses/LICENSE-2.0 */

/* Unless required by applicable law or agreed to in writing, software */
/* distributed under the License is distributed on an "AS IS" BASIS, */
/* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. */
/* See the License for the specific language governing permissions and */
/* limitations under the License. */


#include <stdlib.h>
#include <stdio.h>
#include <string.h>

typedef unsigned long int numb;   
                         /* Should be 64 bit wide, to hold the square of: */
                         /* If you change this, also change "atol" in main */
#define modulus 1073741827
#define multipl 33554467

typedef numb * obj;

numb N=NHASH;     /* Number of objects to hash */
size_t m=8;  /* Size of objects in bytes, rounded up to be a multiple of
               sizeof(numb) */

/*numb k;*/     /* Number of times each object is expected */




/* Some fast way to create funny data: */

numb val = 1234567;

numb next(void)
{
    val = (val * multipl) % modulus;
    return val;
}

void resetvalue(int *numi,int *inum)
{
  int i,j,nval;
  numb dummy;

  val = 1234567;
  /*  nval=N/(*numi);*/
  nval=NHASH;
  for(j=0;j<(*inum-1)*nval;j++){
    for(i=m/sizeof(numb);i>0;i--){
      dummy = next();
    }
  }
    
}


obj newobj(void)
{
    obj o,o2;
    int i;
    o = malloc(m);
    o2 = o;
    for (i = m/sizeof(numb); i > 0;i--) *o2++ = next();
    /*    printf("%d %d\n",o,*o);*/
    return o;
}

numb f(obj o, numb *numi)
/* Our hash function, this should do: */
{
  numb hashlen = 2*NHASH*(*numi)+1;
    numb x = 0;
    int i;
    for (i = m/sizeof(numb); i > 0; i--){
      x += *o++;
    }
    return x % hashlen;
}

void fnew_(obj o, numb *nb, numb *v, numb *numi){
  o = newobj();
  /* get the hash */
  *v=f(o,numi)+1; /*F style counting! */
  *nb=*o;
}

void fresetvalue_(int *numi, int *inum){
  resetvalue(numi,inum);
}


void freeobj_(obj o)
{
  free(o);
}
