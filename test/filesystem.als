module file_system

abstract sig Object {}

sig Name {}

sig File extends Object {} 

sig Dir extends Object {
	contents: Name -> lone Object,
	parent: lone Dir
} 

one sig Root extends Dir {} 

fact ParentDefinition { all d: Dir | d.parent = (contents.d) . Name }

fact RootHasNoParent { all r: Root | no r.parent  }

fact NoOwnAncestor { all d: Dir | d !in d.^parent }

fact RootIsTheRoot { all d: Dir - Root | Root in d.^parent }

assert Lemma1 {
  all o: Dir | no contents.o . Name => no contents.o
}


assert NoDirAliases {
    all o: Dir | lone (contents.o)
} 
check NoDirAliases for 4

fact OneParent {
    all d: Dir | d !in Root => (one d.parent && one contents.d)
}
