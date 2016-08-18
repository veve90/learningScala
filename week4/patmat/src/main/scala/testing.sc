// times
val chars = List('a', 'b', 'a');
println(chars.groupBy(x => x))
val grouped = chars.groupBy(x => x)
val toformat = grouped.map(x => (x._1,x._2.size))
val output = toformat.map{ case(k,v) => (k, v) } toList
//sorting
val freqs = List(('t', 2), ('e', 1), ('x', 3))
val sorted = freqs.sortWith(_._2<_._2)
val l = List("a", "b", "c")
l.lift(1)
l(1)