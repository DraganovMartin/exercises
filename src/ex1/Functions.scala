package ex1

import java.util

object Functions {

  // Връща големината на масив (без да ползва data.length!!!)
  def length(data: List[Int]) ={
    def loop(data: List[Int], lenght: Int): Int = {
      if (data.isEmpty) lenght;
      else loop(data.tail, lenght + 1);
    }

    loop(data, 0);
  }

  // Ако cond е true връща onTrue
  def ifelse(cond: Boolean, onTrue: => Int, onFalse: => Int): Int = {
    if (cond) onTrue else onFalse;
  }

  // Проверява дали скобите в даден масив от символи са балансирани.
  // Коректно: (a)asda(b)(v) | (((a))) | ()(()асдасд)
  // Грешно: )() | ((д) | ((das) (d)( 
  def balance(chars: List[Char]):Boolean={
    def loop(chars: List[Char],openBracketsCnt:Int,closeBracketsCnt:Int): Boolean = {
      if(closeBracketsCnt > openBracketsCnt){
        return false;
      }
      if(chars.isEmpty)
        if(openBracketsCnt == closeBracketsCnt)true else false;
      else {
        if(chars.head == ')')
          loop(chars.tail, openBracketsCnt, closeBracketsCnt+1);
        else if(chars.head == '(')
          loop(chars.tail, openBracketsCnt+1, closeBracketsCnt);
        else loop(chars.tail, openBracketsCnt, closeBracketsCnt);
      }
    }

    if(chars.head == ')') false;
    else loop(chars, 0, 0);
  }

  def map(chars: List[Char], f: (Char) => Any) : Any ={
    def loop(chars: List[Char],f: (Char) =>Any,list:List[Any]): List[Any] ={
      if(chars.isEmpty){
        list
      }
      else {
        val newList = list :+ f(chars.head);
        loop(chars.tail,f,newList)
      }
    }

    loop(chars,f,List());
  }

  def toUpperCase(chars: List[Char]) = {
    def upperCase(char: Char) = {
      if(char >= 97 && char <= 122 )
      (char - 32).toChar;
    }
  
    map(chars, (ch) => upperCase(ch))
  }

  // Проверява дали съществува елемент отговарящ на f
  def exists(data: List[Int], f:(Int) => Boolean) : Boolean = {
    val list = filter(data, x => f(x))//data.filter(x => f(x) )
    if (list.isEmpty) return false;
    else return true
  }

  // Връща масив съдържащ само елементите отговарящи на f
  def filter(data: List[Int], f: (Int) => Boolean) = {
    def loop(data: List[Int], f: (Int) => Boolean, filtered: List[Int]): List[Int] = {
      if (data.isEmpty) filtered;
      else {
        if (f(data.head)) loop(data.tail, f, filtered :+ data.head)
        else loop(data.tail, f, filtered);
      }
    }

    loop(data, f, List());
  }

  // Проверява дали всички елементи отговарят на f
  def forall(data: List[Int], f:(Int) => Boolean) : Boolean = {
    val list = filter(data,x => f(x))//data.filter(x => f(x))
    if (list.size == data.size) return true
    else return false
  }

  // Връща числото от триъгълника на Паскал отговарящо на съответния ред/колона
  def pascal(c: Int, r: Int): Int = {
    if (c >r || c<1 || r<1) return -1;
    def loop(c: Int, r:Int) : Int = {
      if (c == 1 || c == r) return 1;
      else loop(c-1,r-1) + loop(c, r-1);
    }
    loop(c,r)
  }

  def main(args: Array[String]): Unit = {
    val arr = List(1,2,3,4);
    println(length(arr))
    println(ifelse(1==2,1,2))

    val brackets = List('(','a',')','a','s','d','a','(','b',')','(','v',')',' ','|',' ','(','(','(','a',')',')',')',' ','|',' ','(',')','(','(',')','а','с','д','а','д',')');
    println(balance(brackets));

    println(map(List('a','a','b'),_.toInt))

    println(toUpperCase(brackets))

    println(exists(arr,x=>x==3))

    println(filter(arr,x => x %2 == 0))

    println(pascal(2,5))

  }
}
