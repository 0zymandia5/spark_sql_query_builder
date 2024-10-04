package constants

object constants {

  val hierarchy_query_map : Map[String, List[String]] = Map(
    "select" -> List[String](
      "from"      
    ),
    "from" -> List[String](
      "join",
      "where",
      "end"      
    ),
    "join" -> List[String](
      "on"
    ),
    "on" -> List[String](
      "end"
    ),
    "where" -> List[String](
      "end"
    )
  );

  // Variable for query builds
  val queryMap : Map[String, Map[String, List[Any]]] = Map(
    "query_sample_1" -> Map(
        "select"-> List[Any](
            ("geo", "GEO_NAME"),
            ("mkt", "MARKET_NAME"),
            "COUNTRY_NAME"
        ),
        "from"-> List[Any](
            ("Hierarchy","h")
        ),
        "on"-> List[Any](
            ("h.name","=","n.name")
        ),
        "join"-> List[Any](
            ("names","n")
        )
      ),
      "query_err_1" -> Map(
        "select"-> List[Any](
            ("geo", "GEO_NAME"),
            ("mkt", "MARKET_NAME"),
            "COUNTRY_NAME"
        ),
        "from"-> List[Any](
            ("Hierarchy","h")
        )
      )
  );
}