module Behaviours

open System
open TreatzGame

let wander (random: Random) mikishida state  =     

    let currentWanderAngle = state.WanderingAngle + state.RateOfChangeOfDirection * (random.NextDouble() * 2.0 - 1.0)   

    let circlePosition = mikishida.location + (state.CircleDistance * mikishida.velocity.normalize )
    let circleOffset = {
      X = state.CircleRadius * Math.Cos(state.WanderingAngle)
      Y = state.CircleRadius * Math.Sin(state.WanderingAngle) 
      }
    let steeringDirection = ( circlePosition + circleOffset)  - mikishida.location
            
    { state with 
        SteeringDirection = steeringDirection 
        WanderingAngle = currentWanderAngle}

