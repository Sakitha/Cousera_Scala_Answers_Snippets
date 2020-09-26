def product(f: Int => Int,a: Int,b: Int): Int = {
  def product_t(a: Int, acc: Int): Int = {
    if (a > b) acc
    else product_t(a + 1, acc + f(a))
  }
  product_t(a,0)
}
/*
def loop(a:Int,acc:Int): Int = {
  if(a > b) acc
  else loop(a+1, acc + f(a))
}
loop(a,0) */

def sum(f: Int => Int): (Int, Int) => Int ={
  def sumF(a: Int, b: Int): Int = {
    if(a > b) 0
    else f(a) + sumF(a+1, b)
  }
  sumF
}

def sum2(f: Int => Int) (a: Int, b: Int): Int ={
  if(a > b) 0
  else f(a) + sum2(f)(a+1, b)
}

sum2(x => x*x)(1,2)


def general (map: Int => Int) (red: (Int, Int) => Int, zero:Int) (a: Int, b: Int): Int ={
  if(a > b) zero
  else red(map(a) ,general(map)(red,zero)(a+1,b))
}
general(x => x)((x,y) => x+y,0)(1,2)