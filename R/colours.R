## colours to use for each code
row_colours <- function() {
read.csv(text =
"sort_order, team,               code,     R,     G,     B
          1, ,                Rally,     0, 24310, 50358
          2, home, Offense First Ball, 28283, 46266, 65535
          3, home, Offense Transition, 42769, 53759, 65535
          4,   visiting, Offense First Ball, 28283, 46266, 65535
          5,   visiting, Offense Transition, 42769, 53759, 65535
          6, home, Defense First Ball, 28283, 46266, 65535
          7, home, Defense Transition, 42769, 53759, 65535
          8,   visiting, Defense First Ball, 28283, 46266, 65535
          9,   visiting, Defense Transition, 42769, 53759, 65535
         10,              home, Serve, 65355, 40866, 23453
         11,            home, Receive, 65355, 49359, 37940
         12,                home, Set, 65355, 40866, 23453
         13,             home, Attack, 65355, 49359, 37940
         14,              home, Block, 65355, 40866, 23453
         15,                home, Dig, 65355, 49359, 37940
         16,              home, Cover, 65355, 40866, 23453
         17,           home, Freeball, 65355, 49359, 37940
         18,               home, Save, 65355, 40866, 23453
         19,          home, Ball Over, 65355, 49359, 37940
         20,           home, Downball, 65355, 40866, 23453
         21,       home, Substitution, 65355, 49359, 37940
         22,      home, General Error, 65355, 40866, 23453
         23,                visiting, Serve, 65355, 49359, 37940
         24,              visiting, Receive, 65355, 40866, 23453
         25,                  visiting, Set, 65355, 49359, 37940
         26,               visiting, Attack, 65355, 40866, 23453
         27,                visiting, Block, 65355, 49359, 37940
         28,                  visiting, Dig, 65355, 40866, 23453
         29,                visiting, Cover, 65355, 49359, 37940
         30,             visiting, Freeball, 65355, 40866, 23453
         31,                 visiting, Save, 65355, 49359, 37940
         32,            visiting, Ball Over, 65355, 40866, 23453
         33,             visiting, Downball, 65355, 49359, 37940
         34,         visiting, Substitution, 65355, 40866, 23453
         35,        visiting, General Error, 65355, 49359, 37940",
strip.white = TRUE, stringsAsFactors = FALSE)
}
