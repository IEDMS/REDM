#' Calculate the carefulness of a student according to the given obs sequence
#'
#' @param obs an obs vector
#' @param threshold required number of consecutive correct responses which each of students needs to master a skill
#' @param result default carefulness
#'
#' @return the carefulness of student
#' @export
#'
#' @examples bkt.carefulness.obs(c(0,1,1,1,0,1)) -> 0.5
bkt.carefulness.obs <- function(obs.seq, threshold = 3,  result = 0.5) {
  obs.seq = obs.seq[!is.na(obs.seq)]
  obs.seq = paste(obs.seq, collapse = "")
  n.obs = nchar(obs.seq)
  # the given obs.seq must be longer than matched string
  if (n.obs > threshold) {
    m.seq = paste(rep('1', threshold), collapse = "")
    pos = regexpr(m.seq, obs.seq)[1]
    if (pos != -1) {
      obs.seq = substr(obs.seq, pos, n.obs)
      total = nchar(obs.seq) - threshold
      if (total > 0) {
        wrong.pos = unlist(gregexpr("0", obs.seq))
        freq = 0
        if (wrong.pos[1] != -1)
          freq = length(wrong.pos)
        result = 1 - freq / total
      }
    }
  }
  result
}

bkt.carefulness.stu <- function(data.stu, threshold = 3, result = 0.5) {
  result = lapply(data.stu, function(x) apply(x, 1, function(x) bkt.carefulness.obs(x)))
  result = lapply(result, mean)
  
  result
}

bkt.effort <- function(obs.seq, threshold = 3, result = 0.5) {
  
  result
}
