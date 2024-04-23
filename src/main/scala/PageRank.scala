import scala.annotation.tailrec
import scala.util.Random
import scala.collection.parallel.CollectionConverters.*

object PageRank {
    /**
     * @param pages A map of page.id to page for some number of WebPage objects
     * @return      A map of page.id to a weight of 1.0 for those same WebPage objects
     */
    def equal(pages: Map[String, WebPage]): Map[String, Double] = {
        pages.map((s, page) => (s, 1.0))
    }

    /**
     * @param pages A map of page.id to page for some number of WebPage objects
     * @return A map of page.id to a weight that is a simple count of the number of pages linking to that page
     */
    def indegree(pages: Map[String, WebPage]): Map[String, Double] = {
        pages.map((id, page) => (id, countOccurrences(id, page, pages)))
    }

    private def countOccurrences(id: String, page: WebPage, pages: Map[String, WebPage]): Double = {
        pages.foldLeft(0.0)((numPrev, tuple) => numPrev + (if tuple._2.links.contains(page.url) then 1 else 0))
    }

    def pagerank(pages: Map[String, WebPage]): Map[String, Double] = {
        val finalPages = (1 to 10000).par.map(x => jumpPages(pages, pages(getRandomPage(pages.keys.toIndexedSeq))))
        weightPages(finalPages.toList)
    }

    private def getRandomPage(pages: Seq[String]): String = {
        pages(Random.nextInt(pages.length))
    }

    @tailrec
    private def weightPages(finalPages: List[String], weightMap: Map[String, Double] = Map()): Map[String, Double] = {
        finalPages match
            case Nil => weightMap
            case h::tail => weightPages(tail, weightMap + (h -> (weightMap.getOrElse(h, 0.0) + 1)))
    }

    @tailrec
    private def jumpPages(pages: Map[String,WebPage], currentPage: WebPage, counter: Int = 0): String = {
        if counter >= 100 then return currentPage.id

        if currentPage.links.nonEmpty then {
            if Random.nextDouble() < .85 then {
                val links = currentPage.links
                val nextPage = pages.find((id, page) => page.url == getRandomPage(links)).orNull
                jumpPages(pages, if nextPage == null then currentPage else nextPage._2, counter + 1)
            } else {
                jumpPages(pages, pages(getRandomPage(pages.keys.toIndexedSeq)), counter + 1)
            } //Links to follow
        } else jumpPages(pages, pages(getRandomPage(pages.keys.toIndexedSeq)), counter + 1) //No Links
    }
}