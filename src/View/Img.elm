module View.Img exposing (tmp)

import Svg exposing (g, svg)
import Svg.Attributes exposing (class, d, fill, height, points, rx, ry, stroke, strokeLinecap, strokeLinejoin, strokeWidth, version, viewBox, width, y)


tmp n =
    svg
        [ width <| String.fromInt n --"512"
        , height <| String.fromInt n --"512"
        , version "1.0"
        , viewBox "0 0 5120 5120"
        ]
        [ Svg.path
            [ fill "#010101"
            , d "M1108 4865c-15-8-29-23-32-32-3-10-6-702-8-1539l-2-1520-89-21c-260-63-461-245-550-500-26-75-31-106-35-214-4-104-1-140 17-210 69-277 267-480 546-561l80-23h2280l79 23c220 64 394 209 489 407 59 123 72 183 71 340 0 124-3 149-27 220-82 246-267 429-507 503-36 11-82 23-103 27l-37 6 2 667 3 667 35-5c91-13 266-4 348 18 312 84 547 330 618 647 20 90 18 274-5 368-13 56-15 79-7 82 255 70 432 274 453 523 7 81-4 113-44 131-17 8-515 11-1786 11-1523-1-1766-3-1789-15z"
            ]
            []
        , g [ fill "#afa397" ] [ Svg.path [ d "M1232 3243l3-1468 938-3 937-2v1387l-82 48c-55 32-113 78-170 135l-88 87v-619c0-636-1-646-39-675-41-31-113-2-126 50-3 12-4 337-3 723l3 701-57 12c-70 14-180 68-235 115-23 20-44 36-47 36s-7-360-8-799l-3-799-24-26c-33-35-79-35-115 1l-27 26 1 971c0 534 1 1003 0 1041v70l-53 29c-120 67-240 232-269 370-7 31-15 56-18 56s-5-572-5-1271V2168l-28-24c-34-29-76-31-108-5-21 17-25 30-30 111-3 51-4 625-2 1276l3 1184h-351l3-1467zM1140 1424c-195-52-323-234-307-438 12-157 101-286 247-355 49-23 67-26 170-26s121 3 170 26c73 35 127 80 171 142 58 83 74 137 74 253 0 96-2 103-38 176-90 184-290 275-487 222zM2990 1424c-195-52-323-234-307-438 12-157 101-286 247-355 49-23 67-26 170-26s121 3 170 26c73 35 127 80 171 142 58 83 74 137 74 253 0 96-2 103-38 176-90 184-290 275-487 222z" ] [] ]
        , g [ fill "#d1c5bb" ]
            [ Svg.path [ d "M1232 3243l3-1468 938-3 937-2v1387l-82 48c-55 32-113 78-170 135l-88 87v-619c0-636-1-646-39-675-41-31-113-2-126 50-3 12-4 337-3 723l3 701-57 12c-70 14-180 68-235 115-23 20-44 36-47 36s-7-360-8-799l-3-799-24-26c-33-35-79-35-115 1l-27 26 1 971c0 534 1 1003 0 1041v70l-53 29c-116 65-232 220-269 361-11 42-14-147-18-1213l-5-1264-28-24c-34-29-76-31-108-5-21 17-25 30-30 111-3 51-4 625-2 1276l3 1184h-351l3-1467zM1070 1599c-160-17-318-115-414-256-162-239-133-547 73-753 87-87 163-131 277-160 75-19 114-20 1163-20 914 0 1097 3 1156 15 209 44 388 214 450 428 20 70 23 212 5 287-48 212-219 389-431 446-59 16-149 18-1144 18-594 1-1105-1-1135-5zm341-70c153-44 285-172 351-341 31-80 31-249 0-340-51-147-173-272-325-333-65-26-83-29-187-29-103 0-122 3-185 28-163 66-275 184-331 346-28 82-26 234 4 322 98 285 382 431 673 347zm1850 0c153-44 285-172 351-341 31-80 31-249 0-340-51-147-173-272-325-333-65-26-83-29-187-29-103 0-122 3-185 28-163 66-275 184-331 346-28 82-26 234 4 322 98 285 382 431 673 347z" ] [] ]
        , Svg.path [ fill "#fff", d "M1930 4700c0-28 43-118 80-166 116-153 319-203 512-125 83 33 115 31 144-7 22-28 17-86-8-110-54-50-199-92-318-92h-83l7-57c21-181 190-348 371-368 176-20 355 79 433 239 17 35 42 72 54 80 54 38 128-1 128-66 0-71-101-226-193-297-52-39-170-101-195-101-22 0 26-77 93-149 77-83 186-154 289-188 72-24 96-27 211-27 116 0 139 3 210 28 119 41 181 80 271 171 89 89 149 194 179 310 24 92 22 244-4 341l-23 82-61 12c-84 16-161 47-197 77-23 19-30 33-30 61 1 45 17 69 58 83 25 8 37 7 69-9 218-112 472-36 585 175 14 27 29 63 33 81l7 32H3241c-858 0-1311-3-1311-10z" ] []
        ]
