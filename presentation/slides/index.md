- title : DRAGON TREATS: THE BLENDING OF THE PARADIGMS 
- description : A talk about dragons and treats and F#
- author : Andrea Magnorsky, Ross McKinlay
- theme : solarized
- transition : default

***
- data-background : images/dragon.jpg


' thanks for coming to this talk

***
- data-background : images/dragon-treats.png

## DRAGON TREATS: 
### ~ THE BLENDING OF THE PARADIGMS ~ 

### F# Exchange - April 2016

####Andrea           |            Ross

####@silverSpoon     |      @pezi_pink

***
## Dragons Treats

![m](images/treats.jpg)

' how Dragon treats came to be


***

## Treats
![dg](images/dragon-treats.png)
' gameplay 

***
## What is in a game


' how does the game work from a technical point of view
' game loop
' conflict between the two paradigm
' inmutable vs imperative

---

###THE GAME LOOP
![sdl](images/gloop.png)

---
### STATE PROPAGATION
	state
	|> miscUpdates 
	|> updateInputs
	|> updatePositions
	|> collisionDetection
	|> intelligence
	|> testGameOver


***

## Why SDL

![sdl](images/Sdl-logo.png)

' mention ernest
' what is sdl high level
' cross platform

' mention some of the stuff 

***
- data-background : images/sunny-cat.png
- data-background-size : 800px

***
## Artificial stupidity 

## Steering Behaviours : Wander

' dragons need smarts
' Craig Reynolds
' steering behaviour simple to impl because just maths
' produce small random displacements and apply to current direction vector 
' the small changes prevents abrupt changes


---

![d](images/wander_displacement_add_circle_center.png)

---

	type BehaviourState  = {
	  CircleRadius : double
	  CircleDistance : double
	  RateOfChangeOfDirection : double

	  WanderingAngle: double
	  SteeringDirection : Vector2
	  }


---

	let wander (chaos: Random) mikishida state  =     

	    let wanderA = state.WanderingAngle + state.RateOfChangeOfDirection * 
	    				(chaos.NextDouble() * 2.0 - 1.0)     
	    let dist = (state.CircleDistance * mikishida.velocity.normalize )
	    let circlePosition = mikishida.location + dist
	    
	    let circleOffset = { 
	      Vector2.X = state.CircleRadius * Math.Cos(wanderA)
	      Y = state.CircleRadius * Math.Sin(wanderA) 
	      }

	    let steeringDirection = ( circlePosition + circleOffset) 
	                              - mikishida.location
	            
	    { state with SteeringDirection = steeringDirection
	        WanderingAngle = wanderA}


***

![qt](images/secretsquirrel.bmp)

***

## Quad Trees

![qt](images/quadtree.png)

---
	type 'a QuadTree = 
	    | Leaf of  data : 'a list
	    | Branch of
	        data : 'a list *
	        TR : QuadTree<'a> *
	        BR : QuadTree<'a> *
	        BL : QuadTree<'a> *
	        TL : QuadTree<'a> 

***
## Artificial stupidity - Tha return

## Path finding 

---

![f](images/Astar_progress_animation.gif)
' path finding not so 
' shared mutable states
' use sorted set 
' use manhatan distance
' can we use some fewer types 



***

## some sort of conclusion?

***

## Events and User Groups

![fk](images/fk.jpeg)

* [Functional Kats](http://www.meetup.com/nyc-fsharp/)
* [F#unctional Londoners meetup group](http://www.meetup.com/FSharpLondon/)
* Other user groups about programming languages that have no cats with capes on their logos :D

***

### Resources

* [SDL2](https://www.libsdl.org/)
* [Ernest's Tutorials](https://www.youtube.com/playlist?list=PLsmt5lp-6Xodxsh4tu-l0xD3KdYonIwz2)
* [Ernest's SDL Wrapper](https://github.com/hakelimopu/tjofgd-project2/tree/master/Project2/SDL2FS)
* [Steering behaviours](http://gamedevelopment.tutsplus.com/tutorials/understanding-steering-behaviors-wander--gamedev-1624)
* [A* Explanation](http://www.redblobgames.com/pathfinding/a-star/introduction.html)
* [Interesting A* implementation](https://github.com/juhgiyo/EpPathFinding.cs) and [another one](https://github.com/jdoig/A-Star-Pathfinding-in-F-Sharp)


<script>
  (function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){
  (i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),
  m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)
  })(window,document,'script','//www.google-analytics.com/analytics.js','ga');

  ga('create', 'UA-46761189-1', 'auto');
  ga('send', 'pageview');

</script>
