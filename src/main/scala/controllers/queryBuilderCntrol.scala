package controllers

import creationalClasses.queryPartSelection
import constants.constants.hierarchy_query_map
import java.util.ArrayList
import java.{util => ju}

/** <h1>Class queryBuilderCntrol</h1>
 *
 * 
*/
class queryBuilderCntrol {

  private var query_secuency : ju.List[String] = new ArrayList[String]();

  /**
     * <H1>def validate_query_sequence</h1>
     * This functions just validates the secuency of the keys on the query maps
     *  
     * @param queryKeys : Array[String] := Array which contains all query keys in the correct order
     * @param next_key : String  
     * @param prev_key : String 
     * @return Unit
  */
  def validate_query_sequence(queryKeys : Array[String], next_key : String, prev_key : String): Unit = {
    try {
      if(next_key != "end" && queryKeys.contains(next_key) && hierarchy_query_map.get(next_key).get(0).nonEmpty){
        query_secuency.add(next_key);
        for (possible_nxt_key <- hierarchy_query_map.get(next_key)){
          validate_query_sequence(queryKeys, possible_nxt_key.head, next_key);
        }// for closure
      }else if(next_key != "end" && !hierarchy_query_map.get(prev_key).exists(_.contains("end"))){
        query_secuency.clear();
      }// else closure
    } catch {
      case ex: Exception => { println(s"Error at validate_query_sequence function :  ${ex}");}
    }// catch closure
  }// validate_query_sequence closure

  /**
     * <H1>def build_query</h1>
     * This functions builds the query section of SELECT/FROM/JOIN/ON/WHERE
     *  
     * @param names: List[Any]) := which contains the COLUMNS/TABLES names
     * @return String := Contains the part of the query XXXXXXXXX 
  */
  def build_query(queryMap2Build : Map[String, List[Any]]): String = {
    try {
      var queryString : String = "";
      var prev_key : String = "";
      var next_key : String = "select";
      validate_query_sequence(queryMap2Build.keys.toArray, next_key, prev_key);
      if (query_secuency.isEmpty()){
        queryString="Error building the query"
      }else{
        query_secuency.forEach( keyQuery =>{
          val queryPartObj = queryPartSelection(keyQuery);
          queryString += queryPartObj.createQuerySection(queryMap2Build(keyQuery));
        });
      }
      return queryString;
    } catch {
      case ex: Exception => { println(s"Error at build_query function :  ${ex}"); return ""; }
    }
  }//build_query function closure

}//queryBuilderCntrol class closure 