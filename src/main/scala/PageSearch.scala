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
        pages.map(p =>
            numInPages(p, query)
            )
    }


  /**
   * tail recursive counting for number of times any word in the query appears in the page
   * @param p the current page
   * @param query list of words that were queried
   * @param occurrences count of appearances
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
        pages.map(termFrequencyMulti(_, query))
    }

    private def termFrequencyMulti(page: RankedWebPage, query: List[String]): Double = {
      query.foldLeft(0.0)((lastNum, str) => lastNum + termFrequencySingle(page.text, str))
    }

    private def termFrequencySingle(text: String, query: String): Double = {
      numInPage(text, query) / text.length
    }

    /**
     * @param pages a list of RankedWebPage objects to be searched
     * @param query a list of search terms to be counted in those pages
     * @return      a list of the TF-IDF score for each page in the same order given
     */
    def tfidf(pages: List[RankedWebPage], query: List[String]): List[Double] = {
      pages.map(tfidfMulti(_, query, pages))
    }

    private def tfidfMulti(page: RankedWebPage, query: List[String], pages: List[RankedWebPage]): Double = {
      query.foldLeft(0.0)(_ + tfidfSingle(page.text, _, pages))
    }

    private def tfidfSingle(text: String, query: String, pages: List[RankedWebPage]): Double = {
      termFrequencySingle(text, query) * log(pages.length/(numContains(pages, query) + 1))
    }
    
    private def numContains(pages: List[RankedWebPage], query: String): Int = {
      pages.count(_.text.contains(query))
    }
}