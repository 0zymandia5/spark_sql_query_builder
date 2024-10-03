package com.spark.main

import  constants.constants
import controllers.queryBuilderCntrol

object main {
    def main(args: Array[String]): Unit = {
        try {
            val query_builder_cntrol = new queryBuilderCntrol();
            val mapQueries : Map[String, List[Any]] = constants.queryMap("query_err_1");
            val query : String = query_builder_cntrol.build_query(mapQueries);
            println(s"Query to execute : ${query}");
        } catch {
            case ex: Exception => {println("Exception Occurred [main]: "+ex.printStackTrace());}
        }// catch closure
    }// main closure
} // object main closure