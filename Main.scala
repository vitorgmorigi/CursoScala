package recfun

object Main {
  def main(args: Array[String]) {
    // EXERCICIO 1
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }

    // EXERCICIO 2
    print(balance(List('(' ,'a', 'b', '(', 'v', ')', ')')))

    // EXERCICIO 3
      println(countChange(4, List(1,2)))

  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int = {

      if(c < 0 || r < 0 || c > r) throw new IllegalArgumentException("As linhas e/ou colunas precisam ser maior ou igual a zero e a " +
                                                                      "coluna não pode ser maior que a linha!")
      if (c==0 || c==r) 1 else {
        pascal(c - 1, r - 1) + pascal(c, r - 1)
      }
    }
  
  /**
   * Exercise 2
   */

  var openings = 0 // variável que conta aberturas de parenteses
  var closings = 0 // variável que conta fechamentos de parenteses

    def balance(chars: List[Char]): Boolean = {

      if(chars.isEmpty)
        {
          println("true = balancelado; false = desbalanceado ")
          print("Resultado: ")
          closings == openings
        }

      else
        {
          if(closings > openings) // se em algum momento o número de fechamentos for maior que o de aberturas já retorna falso
            false

          else if(chars.head == '(' ) // caso encontre uma abertura
          {
            openings = openings + 1 // incrementa a variavel openings
            println("Achei uma abertura. Total = " + openings)
            balance(chars.tail) // continua varrendo a lista
          }

          else if (chars.head == ')' ) // caso encontre um fechamento
          {
            closings = closings + 1 // incrementa a variavel closings
            println("Achei um fechamento. Total = " + closings)
            balance(chars.tail) // continua varrendo a lista
          }

          else // caso o char da vez nao seja nem '(' nem ')', continua varrendo a lista
            balance(chars.tail) // continua varrendo a lista
        }

    }



  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {

      if (money < 0 || coins.isEmpty) 0

      else if (money == 0) 1

      else countChange(money - coins.head, coins) + countChange(money, coins.tail)

    }
  }
