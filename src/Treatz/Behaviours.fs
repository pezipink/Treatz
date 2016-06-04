module Behaviours

open System
open TreatzGame
open SDLUtility

let wander (random: Random) mikishida state  =     

    let currentWanderAngle = state.WanderingAngle + state.RateOfChangeOfDirection * (random.NextDouble() * 2.0 - 1.0)   
    
    let normalizedDistance = (state.CircleDistance * mikishida.velocity.normalize )
    let circlePosition = mikishida.location + normalizedDistance
    let circleOffset = {
      Vector2.X = state.CircleRadius * Math.Cos(currentWanderAngle) * 1.0<px>
      Y = state.CircleRadius * Math.Sin(currentWanderAngle) * 1.0<px>
      }
    let steeringDirection = ( circlePosition + circleOffset)  - mikishida.location
            
    { state with 
        SteeringDirection = steeringDirection
        WanderingAngle = currentWanderAngle}

        