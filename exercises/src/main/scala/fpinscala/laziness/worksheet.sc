import fpinscala.laziness._

val s = Stream(1, 2, 3, 4)
s.toList_1
s.toList

s.drop(2)
s.drop(2).toList
s.drop(4).toList
s.drop(6).toList
s.drop_1(2)
s.drop_1(2).toList
s.drop_1(4).toList
s.drop_1(6).toList

s.take(2)
s.take(2).toList
s.take(4).toList
s.take(6).toList
s.take_1(2)
s.take_1(2).toList
s.take_1(4).toList
s.take_1(6).toList
s.takeViaUnfold(2)
s.takeViaUnfold(2).toList
s.takeViaUnfold(4).toList
s.takeViaUnfold(6).toList

s.takeWhile(_ < 3).toList
s.takeWhile(_ > 3).toList
s.takeWhileViaFoldRight(_ < 3).toList
s.takeWhileViaFoldRight(_ > 3).toList
s.takeWhileViaUnfold(_ < 3).toList
s.takeWhileViaUnfold(_ > 3).toList

s.forAll(_ > 0)
s.forAll(_ > 3)

s.headOption
Stream().headOption

s.map(_ + 1).toList
s.mapViaUnfold(_ + 1).toList

s.filter(_ % 2 == 0).toList

s.appendElem(5).toList

s.append(Stream(5)).toList

s.flatMap(x => Stream(x,x)).toList
s.flatMapUsingAppend(x => Stream(x,x)).toList

Stream.constant("a").take(5).toList

Stream.from(7).take(5).toList

Stream.fibs.take(10).toList

Stream.constantViaUnfold("a").take(5).toList

Stream.fromViaUnfold(7).take(5).toList

Stream.fibsViaUnfold.take(10).toList

s.zip(s).toList

val s1 = s.append(s)

s.zipAll(s1).toList

s1.startsWith(Empty)
s1.startsWith(s)
s.startsWith(s1)

