module Behaviours

open System
open TreatzGame

type WanderBehaviour  = {
  CircleRadius : double
  CircleDistance : double
  RateOfChangeOfDirection : double

  WanderingAngle: double
  LastDirectionChangeTime : int  
  SteeringDirection : Point
  }

type Vehicle = {
  Velocity: Point
  Position: Point
}

let wander gameTimeSeconds (random: Random) vehicle state  =     
    let timeDiff = gameTimeSeconds = state.LastDirectionChangeTime
    let currentWanderAngle = state.WanderingAngle + state.RateOfChangeOfDirection * (random.NextDouble() * 2.0 - 1.0)   

    let circlePosition = vehicle.Position + (state.CircleDistance * vehicle.Velocity.normalize   )
    let circleOffset = {
      X = state.CircleRadius * Math.Cos(state.WanderingAngle)
      Y = state.CircleRadius * Math.Sin(state.WanderingAngle) 
      }
    let steeringDirection = ( circlePosition + circleOffset)  - vehicle.Position
            
    { state with LastDirectionChangeTime = gameTimeSeconds; SteeringDirection = steeringDirection ; WanderingAngle = currentWanderAngle}

