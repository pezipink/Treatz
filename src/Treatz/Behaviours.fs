module Behaviours

open CommonData
open System
open TreatzGame

let wander (random: Random) mikishida state  =     

    let currentWanderAngle = state.WanderingAngle + state.RateOfChangeOfDirection * (random.NextDouble() * 2.0 - 1.0)   
    
    let normalizedDistance = (state.CircleDistance * mikishida.velocity.normalize )
    let circlePosition = mikishida.location + normalizedDistance
    let circleOffset = {
      Vector2.X = state.CircleRadius * Math.Cos(currentWanderAngle)
      Y = state.CircleRadius * Math.Sin(currentWanderAngle) 
      }
    let steeringDirection = ( circlePosition + circleOffset)  - mikishida.location
            
    { state with 
        SteeringDirection = steeringDirection
        WanderingAngle = currentWanderAngle}

        