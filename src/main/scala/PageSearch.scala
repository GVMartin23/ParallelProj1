import scala.math.log
import scala.collection.parallel.CollectionConverters._
import scala.annotation.tailrec

object PageSearch {
    /**
     * @param pages  a list of RankedWebPage objects to be searched
     * @param query  a list of search terms to be counted in those pages
     * @return       a list of the number of times any of the terms appeared in each page in the same order as given
     */
    def count(pages: List[RankedWebPage], query: List[String]): List[Double] = {
        List() // TODO: implement this method and remove this stub
        pages.map(p =>
            numInPages(p, query)
            )
    }


  /**
   * tail recursive counting for number of times any word in the query appears in the page
   * @param p the current page
   * @param query list of words that were queried
   * @param countSoFar count of appearances
   * @return
   */
    @tailrec
    private def numInPages(p: RankedWebPage, query: List[String], occurrences: Double = 0.0): Double = query match
        case Nil => occurrences
        case h::tail => numInPages(p, tail, occurrences + numInPage(p.text, h.toLowerCase))

    @tailrec
    private def numInPage(text:String, query: String, occurrences: Double = 0.0): Double = {
        val queryIndex = text.indexOf(query)
        queryIndex match
            case -1 => occurrences
            case _ => numInPage(text.substring(queryIndex+query.length), query, occurrences + 1)
    }

    /**
     * @param pages a list of RankedWebPage objects to be searched
     * @param query a list of search terms to be counted in those pages
     * @return      a list of the term-frequency of the occurrences of those terms in each page in the same order given
     */
    def tf(pages: List[RankedWebPage], query: List[String]): List[Double] = {
        List() // TODO: implement this method and remove this stub
    }

    /**
     * @param pages a list of RankedWebPage objects to be searched
     * @param query a list of search terms to be counted in those pages
     * @return      a list of the TF-IDF score for each page in the same order given
     */
    def tfidf(pages: List[RankedWebPage], query: List[String]): List[Double] = {
        List() // TODO: implement this method and remove this stub
    }
}