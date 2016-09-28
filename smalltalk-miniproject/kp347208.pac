| package |
package := Package name: 'kp347208'.
package paxVersion: 1;
	basicComment: ''.


package classNames
	add: #Torus;
	add: #TorusPoint;
	add: #TorusWalk;
	add: #TorusWalkConcat;
	add: #TorusWalkAnamorphic;
	add: #TorusWalkMonadic;
	add: #TorusWalkIterate;
	add: #TorusWalkIterateN;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	yourself).

package!

"Class Definitions"!

Object subclass: #Torus
	instanceVariableNames: 'points shape'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #TorusPoint
	instanceVariableNames: 'value torus coordinates'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Collection subclass: #TorusWalk
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
TorusWalk subclass: #TorusWalkConcat
	instanceVariableNames: 'walks'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
TorusWalk subclass: #TorusWalkAnamorphic
	instanceVariableNames: 'base nextBlock'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
TorusWalk subclass: #TorusWalkMonadic
	instanceVariableNames: 'block walk'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
TorusWalk subclass: #TorusWalkIterate
	instanceVariableNames: 'base block'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
TorusWalk subclass: #TorusWalkIterateN
	instanceVariableNames: 'base block count'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

"End of package definition"!

"Source Globals"!

"Classes"!

Torus guid: (GUID fromString: '{D8697A79-616F-4B8D-8EFC-D5D04082C1D0}')!
Torus comment: ''!
!Torus categoriesForClass!Kernel-Objects! !
!Torus methodsFor!

at: aCollection
	| coordinates |
	coordinates := aCollection copy.
	1 to: (shape size) do: [ :idx |
		coordinates at: idx put: (coordinates at: idx) \\ (shape at: idx)
	].
	^points at: coordinates.!

fill: aCollection
	| grow base |
	shape := aCollection.
	points := Dictionary new.
	base := [ :coordinates |
		| point revCoordinates |
        revCoordinates := coordinates reverse.
		point := TorusPoint torus: self coordinates: revCoordinates.
		points at: revCoordinates put: point.
	].
	grow := [ :acc :dimension | [ :coordinates | 0 to: (dimension - 1) do: [ :coordinate |
		acc value: (coordinates copyWith: coordinate)
	] ] ].
	(shape inject: base into: grow) value: (OrderedCollection new).
	^self!

points
	^points! !
!Torus categoriesFor: #at:!public! !
!Torus categoriesFor: #fill:!private! !
!Torus categoriesFor: #points!public! !

!Torus class methodsFor!

shape: shape
	| torus |
	torus := self new.
	torus fill: shape.
	^(torus points) do: [ :elt | ^elt ]! !
!Torus class categoriesFor: #shape:!public! !

TorusPoint guid: (GUID fromString: '{55057BEF-C1AC-4290-98B9-64277C283288}')!
TorusPoint comment: ''!
!TorusPoint categoriesForClass!Kernel-Objects! !
!TorusPoint methodsFor!

- j
	^self + (j negated)!

@ differences
	| coords |
	coords := coordinates copy.
	1 to: (coords size) do: [ :index |
		coords at: index put: (coords at: index) + (differences at: index)
	].
	^torus at: coords.!

& aBlock
	^TorusWalkAnamorphic base: self nextBlock: aBlock!

% anAssociation
	^TorusWalkIterateN base: self block: [ :point | point + anAssociation value ] count: (anAssociation key)!

+ coordinate
	(coordinate = 0)
		ifTrue: [^self]
		ifFalse: [
			| coords index diff |
			coords := coordinates copy.
			index := coordinate abs.
			diff := coordinate sign.
			coords at: index put: (coords at: index) + diff.
			^torus at: coords.
		]!

| aDirection
	^TorusWalkIterate base: self block: [ :point |
		((point + aDirection) coordinates = self coordinates)
			ifTrue: [nil]
			ifFalse: [point + aDirection]
		].!

coordinates
	^coordinates!

coordinates: coords
	coordinates := coords.
	^coords.!

printOn: aStream
	^value printOn: aStream.!

torus
	^torus!

torus: aTorus
	torus := aTorus.
	^aTorus!

value
	^value!

value: aValue
	value := aValue.
	^aValue! !
!TorusPoint categoriesFor: #-!public! !
!TorusPoint categoriesFor: #@!public! !
!TorusPoint categoriesFor: #&!public! !
!TorusPoint categoriesFor: #%!public! !
!TorusPoint categoriesFor: #+!public! !
!TorusPoint categoriesFor: #|!public! !
!TorusPoint categoriesFor: #coordinates!private! !
!TorusPoint categoriesFor: #coordinates:!private! !
!TorusPoint categoriesFor: #printOn:!public! !
!TorusPoint categoriesFor: #torus!private! !
!TorusPoint categoriesFor: #torus:!private! !
!TorusPoint categoriesFor: #value!public! !
!TorusPoint categoriesFor: #value:!public! !

!TorusPoint class methodsFor!

torus: aTorus coordinates: aCollection
	^(self new)
	    torus: aTorus;
	    coordinates: aCollection;
        yourself.! !
!TorusPoint class categoriesFor: #torus:coordinates:!public! !

TorusWalk guid: (GUID fromString: '{C0C285ED-9E7A-44DB-A16F-FB6F54A67D31}')!
TorusWalk comment: ''!
!TorusWalk categoriesForClass!Collections-Abstract! !
!TorusWalk methodsFor!

, aTorusWalk
	^TorusWalkConcat walks: (OrderedCollection with: self with: aTorusWalk)!

% anAssociation
	^TorusWalkMonadic block: [ :point | point % anAssociation ] walk: self!

| aDirection
	^TorusWalkMonadic block: [ :point | point | aDirection ] walk: self!

add: anElement
	^self shouldNotImplement!

do: aBlock
	^self subclassResponsibility!

first: anInteger
    | answer i |
    answer := OrderedCollection new.
    anInteger > 0 ifFalse: [^answer].
    i := anInteger.
    self do:
        [:each |
        answer add: each.
        i := i - 1.
        i = 0 ifTrue: [^answer]].
    ^answer!

remove: oldElement ifAbsent: exceptionHandler
	^self shouldNotImplement!

species
	^OrderedCollection! !
!TorusWalk categoriesFor: #,!public! !
!TorusWalk categoriesFor: #%!public! !
!TorusWalk categoriesFor: #|!public! !
!TorusWalk categoriesFor: #add:!public! !
!TorusWalk categoriesFor: #do:!public! !
!TorusWalk categoriesFor: #first:!public! !
!TorusWalk categoriesFor: #remove:ifAbsent:!public! !
!TorusWalk categoriesFor: #species!public! !

!TorusWalk class methodsFor!

new
	^self shouldNotImplement!

new: aSize
	^self shouldNotImplement! !
!TorusWalk class categoriesFor: #new!public! !
!TorusWalk class categoriesFor: #new:!public! !

TorusWalkConcat guid: (GUID fromString: '{DF7E8287-92F6-48CC-8A54-092F70E9C60D}')!
TorusWalkConcat comment: ''!
!TorusWalkConcat categoriesForClass!Collections-Abstract! !
!TorusWalkConcat methodsFor!

do: aBlock
	walks do: [ :walk | walk do: aBlock ]!

walks: aWalks
	walks := aWalks! !
!TorusWalkConcat categoriesFor: #do:!public! !
!TorusWalkConcat categoriesFor: #walks:!private! !

!TorusWalkConcat class methodsFor!

walks: aWalks
	^(self basicNew)
		walks: aWalks;
		yourself! !
!TorusWalkConcat class categoriesFor: #walks:!public! !

TorusWalkAnamorphic guid: (GUID fromString: '{EBC8442C-F6A8-43D4-ACD0-5332DAFAE93A}')!
TorusWalkAnamorphic comment: ''!
!TorusWalkAnamorphic categoriesForClass!Collections-Abstract! !
!TorusWalkAnamorphic methodsFor!

base
	^base.!

base: aTorusPoint
	base := aTorusPoint.
    ^aTorusPoint.!

do: aBlock
	aBlock value: base.
	(nextBlock value: base) ifNotNil: [ :walk | walk do: aBlock ]!

nextBlock
	^nextBlock.!

nextBlock: aBlock
	nextBlock := aBlock.
    ^aBlock.! !
!TorusWalkAnamorphic categoriesFor: #base!private! !
!TorusWalkAnamorphic categoriesFor: #base:!private! !
!TorusWalkAnamorphic categoriesFor: #do:!public! !
!TorusWalkAnamorphic categoriesFor: #nextBlock!private! !
!TorusWalkAnamorphic categoriesFor: #nextBlock:!private! !

!TorusWalkAnamorphic class methodsFor!

base: aTorusPoint nextBlock: aBlock
	^(self basicNew)
		base: aTorusPoint;
		nextBlock: aBlock;
		yourself! !
!TorusWalkAnamorphic class categoriesFor: #base:nextBlock:!public! !

TorusWalkMonadic guid: (GUID fromString: '{D763EFE5-8C10-46DA-9C85-050C01D665C7}')!
TorusWalkMonadic comment: ''!
!TorusWalkMonadic categoriesForClass!Collections-Abstract! !
!TorusWalkMonadic methodsFor!

block
	^block!

block: aBlock
	block := aBlock!

do: aBlock
	walk do: [ :point | (block value: point) do: aBlock ].!

walk
	^walk!

walk: aWalk
	walk := aWalk! !
!TorusWalkMonadic categoriesFor: #block!private! !
!TorusWalkMonadic categoriesFor: #block:!private! !
!TorusWalkMonadic categoriesFor: #do:!public! !
!TorusWalkMonadic categoriesFor: #walk!private! !
!TorusWalkMonadic categoriesFor: #walk:!private! !

!TorusWalkMonadic class methodsFor!

block: aBlock walk: aWalk
	^(self basicNew)
		block: aBlock;
		walk: aWalk;
		yourself! !
!TorusWalkMonadic class categoriesFor: #block:walk:!public! !

TorusWalkIterate guid: (GUID fromString: '{2F7EF1A0-933F-43BD-9915-BB00451DEB7A}')!
TorusWalkIterate comment: ''!
!TorusWalkIterate categoriesForClass!Collections-Abstract! !
!TorusWalkIterate methodsFor!

base
	^base!

base: aTorusPoint
	base := aTorusPoint.
	^aTorusPoint!

block
	^block!

block: aBlock
	block := aBlock.
	^aBlock!

do: aBlock
	| current |
	current := base.
	[ current isNil ] whileFalse:
		[  aBlock value: current.
		   current := block value: current.
		]! !
!TorusWalkIterate categoriesFor: #base!private! !
!TorusWalkIterate categoriesFor: #base:!private! !
!TorusWalkIterate categoriesFor: #block!private! !
!TorusWalkIterate categoriesFor: #block:!private! !
!TorusWalkIterate categoriesFor: #do:!public! !

!TorusWalkIterate class methodsFor!

base: aPoint block: aBlock
	^(self basicNew)
		base: aPoint;
		block: aBlock;
		yourself.! !
!TorusWalkIterate class categoriesFor: #base:block:!public! !

TorusWalkIterateN guid: (GUID fromString: '{2F07FEAD-1D25-4039-A7AA-8EEE24237706}')!
TorusWalkIterateN comment: ''!
!TorusWalkIterateN categoriesForClass!Collections-Abstract! !
!TorusWalkIterateN methodsFor!

base
	^base!

base: aPoint
	base := aPoint.
	^aPoint!

block
	^block!

block: aBlock
	block := aBlock.
	^aBlock!

count
	^count!

count: aNumber
	count := aNumber.
	^aNumber!

do: aBlock
	| current |
	current := base.
	count timesRepeat: [
		aBlock value: current.
		current := block value: current.
	]! !
!TorusWalkIterateN categoriesFor: #base!private! !
!TorusWalkIterateN categoriesFor: #base:!private! !
!TorusWalkIterateN categoriesFor: #block!private! !
!TorusWalkIterateN categoriesFor: #block:!private! !
!TorusWalkIterateN categoriesFor: #count!private! !
!TorusWalkIterateN categoriesFor: #count:!private! !
!TorusWalkIterateN categoriesFor: #do:!public! !

!TorusWalkIterateN class methodsFor!

base: aPoint block: aBlock count: anInteger
	^(self basicNew)
		base: aPoint;
		block: aBlock;
		count: anInteger;
		yourself.! !
!TorusWalkIterateN class categoriesFor: #base:block:count:!public! !

"Binary Globals"!

