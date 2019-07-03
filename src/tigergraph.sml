(* Pág 222-223.
Código adaptado de acá https://www.cs.princeton.edu/~appel/modern/ml/chap10/graph.sml
En caso de emergencia ver implementación alternativa en https://www.cs.princeton.edu/~appel/modern/ml/altgraph.html *)

structure tigergraph :> tigergraph =
struct

    open Dynarray

    type node' = int
    type temp = tigertemp.temp
    datatype noderep = NODE of {succ: node' list, pred: node' list}
    type graph = noderep array
    type node = graph * node'

    val emptyNode = NODE{succ=[],pred=[]}

    val bogusNode = NODE{succ=[~1],pred=[]}

    fun isBogus(NODE{succ= ~1::_,...}) = true
    | isBogus _ = false

    fun eq((_,a),(_,b)) = a=b
    fun compare((_,a),(_,b)) = Int.compare(a,b)

    fun augment (g: graph) (n: node') : node = (g,n)

    fun newGraph() = array(0,bogusNode)

    fun nodes g = 
        let val b = Dynarray.bound g
            fun f i = if isBogus( Dynarray.sub(g,i)) 
                      then nil
                      else (g,i)::f(i+1)
        in f 0              
        end

    fun size g = (* binary search for size/first unused node *)
        let fun look(lo,hi) =
                   (* i < lo indicates i in use
                      i >= hi indicates i not in use *)
                if lo=hi then lo
                else let val m = (lo+hi) div 2
                     in if isBogus(Dynarray.sub(g,m)) then look(lo,m) else look(m+1,hi)
                     end
        in look(0, 1 + Dynarray.bound g)
        end

    fun sortedNodes g = (* chap 17 toposort by dfs *)
        let val N = size g
            val mark = Dynarray.array (N, false)
            val sorted = ref []
            fun dfs i = if Dynarray.sub(mark, i) = false then
                let
                    val NODE node = Dynarray.sub(g,i)
                    val _ = Dynarray.update(mark, i, true)
                    val _ = List.app dfs ((#pred) node)
                    val _ = sorted := (g,i)::(!sorted)
                in () end else ()
        in dfs(N-1);
           if N <> List.length (!sorted) then
             raise Fail "[sortedNodes] did not cover all the nodes\n"
           else ();
           !sorted
        end

    fun succ(g,i) = 
        let val NODE{succ=s,...} = Dynarray.sub(g,i) 
        in map (augment g) s 
        end

    fun pred(g,i) = 
        let val NODE{pred=p,...} = Dynarray.sub(g,i)
        in map (augment g) p 
        end

    fun adj gi = pred gi @ succ gi

    fun newNode g = (* binary search for unused node *)
        let val sz = size g in
            Dynarray.update(g,sz,emptyNode);
            (g,sz)
        end

    exception GraphEdge

    fun check(g,g') = (* if g=g' then () else raise GraphEdge *) ()

    fun delete(i,j::rest) = if i=j then rest else j::delete(i,rest)
    | delete(_,nil) = raise GraphEdge

    fun diddle_edge change {from=(g:graph, i),to=(g':graph, j)} = 
      let val _ = check(g,g')
          val NODE{succ=si,pred=pi} = Dynarray.sub(g,i)
          val _ = Dynarray.update(g,i,NODE{succ=change(j,si),pred=pi})
          val NODE{succ=sj,pred=pj} = Dynarray.sub(g,j)
          val _ = Dynarray.update(g,j,NODE{succ=sj,pred=change(i,pj)})
      in ()
      end

    val mk_edge = diddle_edge (op ::)
    val rm_edge = diddle_edge delete
    
    type 'info table = (node,'info) tigertab.Tabla (* Para guardar informacion de los nodos*)

    fun nodename(g,i:int) = "n" ^ Int.toString(i)




    fun print_graph(g) =
     let 
        val ns = nodes g
        fun print_node_info n = 
            let 
                val adjs = succ n
            in  
                print(nodename(n));
                print("=> ");
                List.app (fn a => print(nodename(a)^" ")) adjs;
                print("\n")
            end

        val _ = List.app (fn n => print_node_info n) ns
     in
        ()
    end


end

