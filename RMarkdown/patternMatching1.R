a <- "     <title>The Shawshank Redemption (1994) - Full Cast &amp; Crew - IMDb</title>"

grep(pattern = "        <title>\\(\\.*?\\) \\(\\d+\\) - Full Cast &amp; Crew - IMDb</title>", x = a)
grep(pattern = "\\s+<title>(The Shawshank Redemption) \\(1994\\) - Full Cast &amp; Crew - IMDb</title>",
    x = a)
sub(pattern = "\\s+<title>(The Shawshank Redemption) \\(1994\\) - Full Cast &amp; Crew - IMDb</title>",
     replacement ="\\1", x = a)
grep(pattern = "The Shawshank Redemption", x = a)
grep(pattern = "\\s+<title>(The Shawshank Redemption) \\(1994\\) - Full Cast &amp; Crew - IMDb</title>",  x = a)



grep(pattern = "        <title>(\\w+) \\(\\d+\\) - Full Cast &amp; Crew - IMDb</title>", 
     x = a)
