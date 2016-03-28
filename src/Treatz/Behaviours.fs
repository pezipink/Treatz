module Behaviours

open System
open TreatzGame

type BehaviourState  = {
  CircleRadius : double
  CircleDistance : double
  RateOfChangeOfDirection : double

  WanderingAngle: double
//  LastDirectionChangeTime : int  
  SteeringDirection : Point
  }

//type Vehicle = {
//  Velocity: Point
//  Position: Point
//}

let wander  (random: Random) mikishida state  =     
//    let timeDiff = gameTimeSeconds = state.LastDirectionChangeTime
    let currentWanderAngle = state.WanderingAngle + state.RateOfChangeOfDirection * (random.NextDouble() * 2.0 - 1.0)   

    let circlePosition = mikishida.location + (state.CircleDistance * mikishida.velocity.normalize   )
    let circleOffset = {
      X = state.CircleRadius * Math.Cos(state.WanderingAngle)
      Y = state.CircleRadius * Math.Sin(state.WanderingAngle) 
      }
    let steeringDirection = ( circlePosition + circleOffset)  - mikishida.location
            
    { state with 
//    LastDirectionChangeTime = gameTimeSeconds; 
        SteeringDirection = steeringDirection 
        WanderingAngle = currentWanderAngle}

