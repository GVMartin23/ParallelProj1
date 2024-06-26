import scala.math

// NOTE: This object is implemented for you and does not need to be adjusted
object SearchedWebPageNormalize {
    /**
     * Transforms a list of SearchedWebPage to have new normalized values for weight and textmatch
     * @param pages a list of SearchedWebPage objects that have not been normalized
     * @return      a list of SearchedWebPage objects that have been normalized
     */
    def normalize(pages: List[SearchedWebPage]): List[SearchedWebPage] = {
        // extract lists of weights and matches from the pages
        val weights = (for page <- pages yield page.weight)
        val matches = (for page <- pages yield page.textmatch)

        // use minimum and maximum values to create a normalizing closures
        def normWeight(x: Double): Double = (x-weights.min)/(weights.max-weights.min+0.00001)
        def normMatch(x: Double): Double = (x-matches.min)/(matches.max-matches.min+0.00001)

        // use these helper functions to normalize pages
        (for (page <- pages) yield 
            new SearchedWebPage(
                page.id,
                page.name,
                page.url,
                page.text,
                page.links,
                normWeight(page.weight),
                normMatch(page.textmatch)
            )
        )
    }
}

// NOTE: This is a bogus sorting option that ignores our search weights and just uses alphabetical order on name
// This implementation exists only to give you an idea of what is required in your other objects
object NameOrdering extends Ordering[SearchedWebPage] {
    def compare(a: SearchedWebPage, b: SearchedWebPage): Int = a.name compare b.name
}

object ArithmeticOrdering extends Ordering[SearchedWebPage] {
    def compare(a: SearchedWebPage, b: SearchedWebPage): Int = mean(a.weight, a.textmatch) compare mean(b.weight, b.textmatch)
    
    private def mean(a: Double, b:Double): Double = (a + b) /2
}

object GeometricOrdering extends Ordering[SearchedWebPage] {
    def compare(a: SearchedWebPage, b: SearchedWebPage): Int = Math.sqrt(a.weight*a.textmatch) compare Math.sqrt(b.weight*b.textmatch)
}

object HarmonicOrdering extends Ordering[SearchedWebPage] {
    def compare(a: SearchedWebPage, b: SearchedWebPage): Int = harmonic(a.weight, a.textmatch) compare harmonic(b.weight, b.textmatch)
    
    private def harmonic(a: Double, b: Double): Double = 2/((1/a)+(1/b))
}
