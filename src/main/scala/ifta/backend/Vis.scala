package ifta.backend


import ifta._
import ifta.analyse.{Solver,Simplify}


/**
  * Created by guille on 21/11/16.
  */
object Vis {

  def apply(iFTA: IFTA):String = {
    val edges = iFTA.edges.zipWithIndex
    val featsNotused = iFTA.feats -- iFTA.fm.feats.toSet
    val fm =
      if (featsNotused.isEmpty) iFTA.fm
      else  iFTA.fm && (featsNotused.foldLeft[FExp](FNot(FTrue))(_ || Feat(_)) || Feat("__feat__"))
    val fsOptions = getSols(fm,edges.map(e => (e._2,e._1.fe)).toMap)
    mkVis(mkIFTAData(iFTA),fsOptions)
  }

  def connector(nIFTA: NIFTA):String = {
    val graph = connectorDot(nIFTA)
    val edges = graph._2
    val fs = getSols(nIFTA.fm,edges.toSeq)
    mkVis(mkConnData(graph._1.replaceAll("[\"]","""\\"""").replaceAll("[\n\r]", "")),fs)
  }

  private def mkVis(data:String,fsOptions:Map[Int, List[(Int, Boolean)]]):String = {
    s"""
       |<!DOCTYPE HTML>
       |<html>
       |<head>
       |    <script type="text/javascript" src="https://cdnjs.cloudflare.com/ajax/libs/vis/4.17.0/vis.min.js"></script>
       |    <link href="https://cdnjs.cloudflare.com/ajax/libs/vis/4.17.0/vis.min.css" rel="stylesheet" type="text/css" />
       |
       |<style type="text/css">
       |
       |html,body {
       |  width:100%;
       |  height:100%;
       |}
       |#mynetwork {
       |  float: left;
       |  width: 67%;
       |  height: 100%;
       |  padding: 1em;
       |}
       |#controls{
       |  float:left;
       |  width:27%;
       |  height:100%;
       |  padding: 1em;
       |}
       |</style>
       |</head>
       |<body onload="draw();">
       |<div id="mynetwork"></div>
       |<div id="controls">
       |<h2>Feature Selection</h2>
       |
       |${mkFSOptions(fsOptions)}
       |
       |</div>
       |
       |<script type="text/javascript">
       |var data, options;
       |
       |${initFSOptions(fsOptions)}
       |
       |${mkUpdateFs()}
       |
       |${mkDraw(data)}
       |
       |</script>
       |</body>
       |</html>
     """.stripMargin
  }

  private def initFSOptions(sols:Map[Int,List[(Int,Boolean)]]) =
    s"""
       | var fsArray = [${sols.map(s => s"""['${s._1}',[${s._2.map(e => s"""['${e._1}','${e._2}']""").mkString("",",","")}]]""").mkString(",")}];
       | var FS = new Map(fsArray)
       |
       """.stripMargin

  private def mkFSOptions(sols:Map[Int,List[(Int,Boolean)]]) = {
    s"""
       |<form id="optionsFS" onclick="updateFS()">
       | <input type="radio" name="fss" value="all" checked> All<br>
       |${sols.map(s => s"""<input type="radio" name="fss" value="${s._1}"> FS${s._1} <br>""").mkString("\n")}
       |</form>
     """.stripMargin
  }

  private def mkUpdateFs() = {
    s"""
       |function updateFS(){
       |  var fsOptions = document.getElementsByName('fss');
       |  var selection;
       |
       |  for (var i = 0, length = fsOptions.length; i < length; i++) {
       |    if (fsOptions[i].checked) {
       |        selection = fsOptions[i].value
       |        break;
       |    }
       |  }
       |
       |  if (selection == 'all') {
       |    // for (var e of data.edges) {
       |    for (var id of data.edges.getIds()){
       |      data.edges.update({
       |        id:id,
       |        color: 'black'
       |      });
       |    }
       |  } else {
       |    for (var [key, value] of FS.get(selection)) {
       |        data.edges.update({
       |          id: key,
       |          color: (value == 'true') ? 'black' : '#cccccc'
       |        });
       |    }
       |  }
       |}
     """.stripMargin

  }

  private def mkDraw(initData:String) = {
    s"""
       |function draw(){
       |  var container = document.getElementById('mynetwork');
       |
       | ${initData}
       |
       |  options = {
       |    nodes: {
       |      shape: 'box',
       |      color: {background:'white', border: 'black'}
       |    },
       |    edges: {
       |      color: 'black',
       |      smooth: {
       |        forceDirection: 'none',
       |        roundness: 0.50,
       |        type:'continues'
       |      }
       |    },
       |    physics: {
       |      enabled:true,
       |      solver: 'forceAtlas2Based',//'repulsion',
       |      maxVelocity: 50,
       |      minVelocity: 0.1//,
       |      // repulsion: {
       |      //   centralGravity: 0.3,
       |      //   springLength: 10,
       |      //   springConstant: 0.5,
       |      //   damping: 0.69
       |      // }
       |    },
       |    layout: {
       |      hierarchical: {
       |        levelSeparation: 50,
       |        treeSpacing: 100,
       |        enabled:false,
       |        direction: 'LR',
       |        sortMethod: 'directed',
       |        edgeMinimization: true,
       |        parentCentralization: false
       |      }
       |    },
       |    configure:true
       |  }
       |  // create a network
       |  var network = new vis.Network(container, data, options);
       |
       |  network.on("configChange", function() {
       |        // this will immediately fix the height of the configuration
       |        // wrapper to prevent unecessary scrolls in chrome.
       |        // see https://github.com/almende/vis/issues/1568
       |        var div = container.getElementsByClassName('vis-configuration-wrapper')[0];
       |        div.style["height"] = div.getBoundingClientRect().height + "px";
       |      });
       |}
     """.stripMargin
  }

  private def getSols(fm:FExp,edges:Iterable[(Int,FExp)]): Map[Int, List[(Int, Boolean)]] ={
    var sols = Solver.all(fm).map(_.filterNot(_._1 == "__feat__")).toSet.toList.zipWithIndex
    var res:Map[Int,List[(Int,Boolean)]] = Map()
    var templ: List[(Int,Boolean)] = List()

    for ((s,idsol) <- sols) {
      templ = List()
      for ((idedge, fe) <- edges)
        templ ::= (idedge, fe.check(s))
      res += idsol -> templ
    }
    res
  }

  private def mkIFTAData(iFTA: IFTA):String = {
    val edges = iFTA.edges.zipWithIndex
    s"""
       |options = {};
       |
       |data = {
       |  nodes: new vis.DataSet(options),
       |  edges: new vis.DataSet(options)
       |}
       |
       |data.nodes.add([
       |${iFTA.locs.map(l=>s"""{id:$l,label:'${iFTA.aps.getOrElse(l,l)}'}""").mkString(",")}
       |]);
       |
       |data.edges.add([
       |${(for ((e,i) <- edges) yield mkIFTAEdge(e,i,iFTA)).mkString(",")}
       |]);
       |
       |
     """.stripMargin
  }

  private def mkIFTAEdge(e:Edge,id:Int,i:IFTA):String = {
    var label:List[String] = List()
    if (e.fe != FTrue)      label ::= Show(e.fe)
    if (e.cReset.nonEmpty)  label ::= e.cReset.map(_ + ":=0").mkString(",")
    if (e.cCons != CTrue)   label ::= Show(e.cCons)
    if (!e.act.isEmpty)     label ::= e.act.mkString("/")
    s"""{id:$id,from:'${e.from}',to: '${e.to}', label:'${label.mkString(",")}'}"""
  }

  private def mkConnData(dot:String):String = {
    //    val dot = connectorDot(nIFTA)._1.replaceAll("[\"]","""\\"""").replaceAll("[\n\r]", "")
    s"""
       |var parsedData = vis.network.convertDot("${dot}");
       |
     |  data = {
       |    nodes: new vis.DataSet(parsedData.nodes),
       |    edges: new vis.DataSet(parsedData.edges)
       |  }
       |
     |  options = parsedData.options;
       |
   """.stripMargin
  }


  private def connectorDot(nIFTA: NIFTA): (String, Map[Int, FExp]) = {
    var sol = getConnEdges(nIFTA,0,Map())
    val res =
      s"""digraph G {
        |  rankdir=LR;
        |  node [margin=0.1 width=0.3 height=0.2 shape=box]
        |  edge [arrowsize=0.7]
        |  ${sol._1}
        |}
    """.stripMargin
    (res,sol._2)
  }

  private def getConnEdges(nIFTA: NIFTA,i:Int,mm:Map[String,(Int,Boolean,FExp)]) = {
    var res = ""
    var l = i
    var edgeId:Int = 0
    var edges:Map[Int,FExp] = Map()
    var m = mm
    for (ifta <- nIFTA.iFTAs) {
      var lastL = l
      if (ifta.shortname!="")
        res += s"""\n  $l [label="${ifta.shortname}"]"""
      else if (ifta.in.nonEmpty || ifta.out.nonEmpty)
        res += s"""\n  $l [label="${(ifta.in++ifta.out).mkString("-")}"]"""

      for (in <- ifta.in) {
        if (m contains in) {
          res += s"""\n  ${lastL+1} [shape=text, label="$in"] """
          res += s"""\n  ${m(in)._1} -- ${lastL+1} [id="$edgeId"]"""
          res += s"""\n  ${lastL+1} -> $l [id="${edgeId+1}"]"""
          edges += edgeId -> m(in)._3
          edges += (edgeId+1) -> ifta.fEPort(in)
          edgeId = edgeId + 2
          lastL = lastL +1
          m -= in
        }
        else m += (in->(l,true,ifta.fEPort(in)))
      }
      for (out <- ifta.out) {
        if (m contains out) {
          res += s"""\n  ${lastL+1} [shape=text, label="$out"] """
          res += s"""\n  $l -- ${lastL+1} [id="$edgeId"]"""
          res += s"""\n  ${lastL+1} -> ${m(out)._1} [id="${edgeId+1}"]"""
          edges += edgeId -> ifta.fEPort(out)
          edges += (edgeId+1) -> m(out)._3
          edgeId = edgeId + 2
          lastL = lastL+1
          m -= out
        }
        else m += (out->(l,false,ifta.fEPort(out)))
      }
      l = lastL+1
    }
    for ((k,(l,b,fe)) <- m) {
      res += s"""\n  { node [shape=ellipse,margin=0]  $k }"""
      if(b) {
        res += s"""\n  "$k" -> $l [id=$edgeId]"""

      }
      else  {
        res += s"""\n  $l -> "$k" [id=$edgeId]"""
        edges += edgeId -> fe
      }
      edgeId = edgeId +1
    }
    (res,edges)
  }


}
