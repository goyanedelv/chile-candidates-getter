VoteGermany2013 <- data.frame(Party=c( "CDU/CSU", "SPD", "LINKE","GRUENE"),
                          Result=c(311,193,64,63))


 seats <- function(N,M, r0=2.5){ 
 radii <- seq(r0, 1, len=M)

 counts <- numeric(M)
 pts = do.call(rbind,
            lapply(1:M, function(i){
              counts[i] <<- round(N*radii[i]/sum(radii[i:M]))
              theta <- seq(0, pi, len = counts[i])
              N <<- N - counts[i]
              data.frame(x=radii[i]*cos(theta), y=radii[i]*sin(theta), r=i,
                         theta=theta)
            }  )
  )
   pts = pts[order(-pts$theta,-pts$r),]
   pts
 }


election <- function(seats, counts){
stopifnot(sum(counts)==nrow(seats))
seats$party = rep(1:length(counts),counts)
seats
}

layout = seats(631,16)
result = election(layout, VoteGermany2013$Result) # no overall majority!!!
plot(result$x, result$y, col=result$party,pch=19, asp=1)