package laas.fspex.spp

import laas.fspex.model.WeightedGraph
import scala.collection.mutable
import scala.collection.mutable._
import scala.util.Random


/**
 * Created by Ulrich Matchi Aïvodji on 23/06/2015.
 */
object Utils{

  def nodeTovertex(graph:WeightedGraph):(mutable.Map[Int,String],mutable.Map[String,Int])={

    var map=mutable.Map[Int,String]()
    var reversed_map=mutable.Map[String,Int]()

    var id=0

    for(node<-graph.nodes){
      map(id)=node.me.name
      reversed_map(node.me.name)=id
      id+=1
    }
    (map,reversed_map)

  }

  def nodeTovertex(graph:WeightedGraph,n:Int):(mutable.Map[Int,String],mutable.Map[String,Int])={

    var map=mutable.Map[Int,String]()
    var reversed_map=mutable.Map[String,Int]()

    var id=0

    val gen=Random

    for(j<-1 to n){
      val node=graph.nodes(gen.nextInt(graph.nodes.size))
      map(id)=node.me.name
      reversed_map(node.me.name)=id
      id+=1
    }
    (map,reversed_map)

  }

}

class FloydWarshall[G <: WeightedGraph](graph: G) {




  def compute():(mutable.Map[Int,String],mutable.Map[String,Int],Array[Array[Double]])={



    val (map,reversed_map)=Utils.nodeTovertex(graph)

    val m=map.size

    var dist = Array.fill[Double](m,m){Double.MaxValue}

    for(v<-0 to m-1){
      dist(v)(v)=0
    }

    for(edge <- graph.edges){
      val u=reversed_map(edge.a.me.name)
      val v=reversed_map(edge.b.me.name)
      dist(u)(v)=edge.getWeight
    }

    for(k<-0 to m-1){
      for(i<-0 to m-1){
        for(j<-0 to m-1){
          if (dist(i)(j) > dist(i)(k) + dist(k)(j)){
            dist(i)(j)=dist(i)(k) + dist(k)(j)
          }
        }
      }
    }

    (map,reversed_map,dist)

  }

  def compute(n:Int):(mutable.Map[Int,String],mutable.Map[String,Int],Array[Array[Double]])={


    val (mapAll,reversed_mapAll)=Utils.nodeTovertex(graph)

    val (map,reversed_map)=Utils.nodeTovertex(graph,n)



    val m=map.size

    var dist = Array.fill[Double](m,m){Double.MaxValue}

    var v=0

    while(v<m){
      dist(v)(v)=0.0
      v+=1
    }


    for(edge <- graph.edges){
      val u=reversed_mapAll(edge.a.me.name)
      val v=reversed_mapAll(edge.b.me.name)
      
      if (map.contains(u)&map.contains(v)){
        dist(u)(v)=edge.getWeight
      }

    }

    for(k<-0 to m-1){
      for(i<-0 to m-1){
        for(j<-0 to m-1){
          if (dist(i)(j) > dist(i)(k) + dist(k)(j)){
            dist(i)(j)=dist(i)(k) + dist(k)(j)
          }
        }
      }
    }

    (map,reversed_map,dist)

  }

}

