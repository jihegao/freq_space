extensions [table nw]

breed [ units unit ]
breed [ services service ]
breed [ crew person ]

globals [
  service_schedule
]

units-own [
  id
  service_requests
]

services-own [
  id
  caller          ; unit
  is_start_node?  ; true - start node, false - end node
  transitions
  transitions_code
  crew_demand
  origin_trigger_time
]


to-report total_service_time
  report max [xcor] of services
end


to-report init-task-pair [ tid tag crew_dem ]
  let start nobody
  let pair no-turtles
  hatch 1 [
    set breed services
    set caller myself
    set id tid
    set is_start_node? true
    set crew_demand crew_dem
    set start self
    set label (word "<" id " " tag ">")
    set label-color black
    set heading 90
    set ycor 10 + 2 * [ycor] of caller + id + count services with [label = [label] of myself]
    set shape "arrow"
    set pair (turtle-set pair self)
  ]
  hatch 1 [
    set breed services
    set caller myself
    set id tid
    set is_start_node? false
    set crew_demand crew_dem
    set label (word "</" id " " tag ">")
    set label-color black
    set color [color] of start
    set heading 90
    set ycor [ycor] of start
    set shape "circle"
    create-link-from start
    set pair (turtle-set pair self)
  ]
  report pair
end

to-report all_demand_list [service_pack]
  let schedule table:from-list map [c -> (list first c table:from-list (list (list 0 last c))) ] table:to-list config_crew
  let all_demands sentence [map [k -> (list k (list xcor (- table:get crew_demand k) ))] table:keys crew_demand] of service_pack with [is_start_node?]
                           [map [k -> (list k (list xcor table:get crew_demand k))] table:keys crew_demand] of service_pack with [not is_start_node?]
  report all_demands
end

to-report all_service_demands [service_pack]
  let schedule table:from-list map [c -> (list first c table:from-list (list (list 0 last c))) ] table:to-list config_crew
  let all_demands all_demand_list service_pack
  foreach all_demands [ demands ->
    foreach demands [ d ->
      let k first d
      let rec (last d)
      let rec_time first rec
      let rec_num_change last rec
      let records table:get schedule k
      ifelse table:has-key? records rec_time [
        table:put records rec_time rec_num_change + (table:get records rec_time)
      ][
        table:put records rec_time rec_num_change
      ]
    ]
  ]
  report schedule
end

to-report crew_schedule [service_pack]
  let cumulated_schedule table:make
  foreach table:keys (all_service_demands service_pack) [ ctype ->
    let records table:get (all_service_demands service_pack) ctype
    let num 0
    let sorted []
    foreach sort table:keys records [ time_stamp ->
      set num (num + table:get records time_stamp)
      set sorted lput (list time_stamp num) sorted
    ]
    table:put cumulated_schedule ctype sorted
  ]
  report cumulated_schedule
end


to setup
  clear-all
  ask patches [ set pcolor white ]

  create-units 4 [
    set id who
    set color green - id
    set service_requests no-turtles
    set ycor 25 * who

    foreach table:values config [ task ->
      if not member? table:get task "id" [id] of service_requests [
        set service_requests (turtle-set service_requests init-task-pair (table:get task "id") (table:get-or-default task "tag" "") (table:get-or-default task "crew" "") )
      ]
    ]

    ask service_requests [
      if not member? id (table:keys config) [ die ]
      ifelse trigger_time > 0 [
        set xcor trigger_time
      ][
        set transitions_code (table:get my_config (ifelse-value (is_start_node?)[ "start" ][ "end" ]))
        set transitions map [ cond -> interpret cond ] transitions_code
      ]
    ]
  ]

  foreach map [t -> [(list label trigger_time transitions)] of t ] sort services show
end


to compile [c_option]
  foreach sort services [ s ->
    ask s [
      set xcor trigger_time
      set xcor xcor + 2
    ]
  ]
  (ifelse
    c_option = "FIFO" [
      let service_pack no-turtles
      foreach sort services [ s ->
        ask s [
          set xcor trigger_time
          while [is_start_node? and not crew_available? service_pack][
            set xcor xcor + 1
          ]
        ]
        set service_pack (turtle-set service_pack s)
      ]
    ]
    [
      let service_pack no-turtles
      ask units [
        foreach sort service_requests [ s ->
          set service_pack (turtle-set service_pack s)
          ask s [
            set xcor max (list trigger_time )
            while [is_start_node? and not crew_available? service_pack][
              set xcor xcor + 1
            ]
          ]
        ]
      ]
    ]
  )
end

to-report crew_available? [service_pack]
  let available true
  let crew_types table:keys crew_demand
  foreach crew_types [ ctype ->
    if (crew_capacity service_pack ctype xcor) < table:get crew_demand ctype [
      set available false
    ]
  ]
  report available
end

to-report crew_capacity [ service_pack ctype time ]
  report last last filter [c_log -> first c_log <= time] (table:get (crew_schedule service_pack) ctype)
end

;to-report crew_available_time [service_pack start]
;  let crew_types table:keys crew_demand
;  let capacity map [ ctype -> (crew_capacity service_pack ctype start) - table:get crew_demand ctype ] crew_types
;end

to-report init-crew-arrangement
  let plan []
  let time 0
  foreach sort units [
    u ->
    ask u [
      foreach sort service_requests [
        s ->
        set plan lput s plan
      ]
    ]
  ]

  report plan
end


to-report my_start_node
  report one-of services with [id = [id] of myself and is_start_node? and caller = [caller] of myself]
end

to-report get_des_node [ tid is_start? ]
  report one-of services with [id = tid and is_start_node? = is_start? and caller = [caller] of myself]
end

to-report interpret [ cond ]
  ;    { "func": "after-task", "params":[ 0, "start"] }  -->  [ -> [trigger_time] of (turtle 0) ]  ; if trigger_time of my_des_node not yet computed
  ;    { "func": "after-task", "params":[ 0, "end] }     -->  [ -> [trigger_time] of (turtle 1) ]  ; if trigger_time of my_des_node not yet computed
  ;    { "func": "time-after", "params": 9 }             -->  [ -> [trigger_time] of (turtle 8) + 9 ] if trigger_time of my_start_node not yet computed
  ;   ...
  let ref_node nobody

  set ref_node (ifelse-value
    (table:get cond "func" = "time-after")[ my_start_node ]
    (table:get cond "func" = "after-task")[
      ifelse-value ( last table:get cond "params" = "start" )
      [ get_des_node (first table:get cond "params") true ]
      [ get_des_node (first table:get cond "params") false]
    ][
      "function not defined"
    ]
  )

  ; check loop in network
  if (nw:path-to ref_node != false) [
    error (word "loop exists among refs " (list [list id start_end_sign is_start_node?] of self [list id start_end_sign is_start_node?] of ref_node))
  ]
  create-link-from ref_node

  report (ifelse-value
    (table:get cond "func" = "time-after")
    [ (runresult
          (word "[ -> " (get_params_value (table:get cond "params")) " + "
                     ifelse-value (ref_node = self) [ 0 ][ word "[trigger_time] of " ref_node ]
                " ]"
          )
      )
    ]
    [ (runresult (word "[ -> [trigger_time] of " ref_node " ]" )) ]
  )
end

to-report get_params_value [params]
  ifelse (is-number? params)[
    report params
  ][
    let func (runresult (word "[ [vars] -> " (table:get params "func") " vars ]"))
    report (runresult func (table:get params "params") )
  ]
end

to-report random_normal [ vars ]
  report random-normal first vars last vars
end


to-report trigger_time
  let ttime ifelse-value (is-number? table:get my_config (start_end_sign is_start_node?)) [
    table:get my_config ifelse-value (is_start_node?)[ "start" ]["end"]
  ][
    ifelse-value (transitions = 0)
    [ 0 ]
    [ max_duration ]
  ]
  if (xcor > ttime)[ set origin_trigger_time ttime ]
  report ifelse-value (xcor > ttime)[ xcor ][ ttime ]
end

to-report start_end_sign [ bool ]
  report ifelse-value bool [ "start" ][ "end" ]
end

to-report max_duration
  report max map [ tran -> (runresult tran) ] transitions
end


to-report config_crew
  report table:get (table:from-json-file "input.json") "crew"
end

to-report config
  let result table:make
  carefully [
    foreach table:get (ifelse-value load-from-file? [ table:from-json-file "input.json" ] [ table:from-json config_input ]) "services" [ conf ->
      table:put result (table:get conf "id") conf
    ]
  ]
  [  ]
  report result
end


to-report my_config
  report table:get config id
end
@#$#@#$#@
GRAPHICS-WINDOW
349
68
1343
526
-1
-1
2.237
1
10
1
1
1
0
0
0
1
0
440
0
200
0
0
1
ticks
30.0

BUTTON
14
10
80
43
NIL
setup
NIL
1
T
OBSERVER
NIL
S
NIL
NIL
1

INPUTBOX
13
45
345
643
config_input
{\"services\":[\n {\n    \"id\":0,\n    \"tag\":\"外观检查\",\n    \"start\":1,\n    \"end\": 10,\n    \"crew\": { \"a\": 2 }\n  },\n  {\n    \"id\":1,\n    \"tag\":\"加油\",\n    \"start\": [\n      { \"func\": \"after-task\", \"params\": [0, \"start\"] }\n    ],\n    \"end\": [\n      { \"func\": \"after-task\", \"params\": [0, \"end\"] }\n    ],\n    \"crew\": { \"a\": 2 }\n  },\n  {\n    \"id\":2,\n    \"tag\":\"挂弹\",\n    \"start\": [\n      { \"func\": \"after-task\", \"params\": [0, \"end\"] },\n      { \"func\": \"after-task\", \"params\": [1, \"end\"] }\n    ],\n    \"end\": [\n      { \"func\": \"time-after\", \"params\":\n          { \"func\": \"random_normal\", \"params\": [15, 2] }\n      }\n    ],\n    \"crew\": { \"a\": 2 }\n  },\n  {\n    \"id\":3,\n    \"tag\":\"推出\",\n    \"start\":[\n      { \"func\": \"after-task\", \"params\": [2, \"end\"] }\n    ],\n    \"end\": [\n      { \"func\": \"time-after\", \"params\":\n          { \"func\": \"random_normal\", \"params\": [20, 2] }\n      }\n    ],\n    \"crew\": { \"a\": 2 }\n  }\n]}
1
1
String

BUTTON
265
12
389
45
NIL
compile option
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SWITCH
99
12
261
45
load-from-file?
load-from-file?
0
1
-1000

CHOOSER
390
10
528
55
option
option
"FIFO" "SEQUENCE"
0

MONITOR
536
12
661
57
NIL
total_service_time
17
1
11

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

line-arrow
true
0
Line -7500403 true 150 0 150 150
Line -7500403 true 105 45 150 0
Line -7500403 true 195 45 150 0

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.4.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
