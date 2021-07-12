# Copyright (c) 2020 Reed A. Cartwright
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are
# met:
#
#     Redistributions of source code must retain the above copyright
#     notice, this list of conditions and the following disclaimer.
#
#     Redistributions in binary form must reproduce the above copyright
#     notice, this list of conditions and the following disclaimer in
#     the documentation and/or other materials provided with the
#     distribution.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
# A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
# HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
# SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
# LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
# DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
# THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
# (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
# OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

library(tidyverse)
library(rbedrock)

# find_wither_killers will search a rectangle of block coordinates for any chunks
# with water between y = 0 and y = 4. It outputs a table containing the NW
# coordinate of the chunk and the number of water blocks at bedrock level.
# These chunks typically have a pond that has generated at bedrock level and
# removed bedrock blocks. You should visit these chunks in a creative copy
# and look for a spot the spawn the wither. The best spots have a 3-high vertical
# wall of bedrock in the positive x direction (east).
#
# USAGE EXAMPLE:
# source("https://git.io/JkPRt") # read this script from the web
# dbpath <- PATH_TO_WORLD_HERE # set to your world 
#                              # run list_worlds() if you don't know it.
# tbl <- find_wither_killers(dbpath, -100, -100, 100, 100) # search in a 201 x 201 block
#                                                          #  area centered at 0,0
# print(tbl) # print your findings
# write_csv(tbl, 'wither_killers.csv')

find_wither_killers <- function(dbpath, west, north, east, south, dimension = 0) {
    # Open database.
    db <- try(bedrockdb(dbpath))
    if(inherits(db, "try-error")) {
        stop("cannot open world database")
    }
    on.exit(db$close())

    # convert block coordinates to chunk coordinates
    x <- seq.int(west %/% 16, east %/% 16)
    z <- seq.int(north %/% 16, south %/% 16)

    g <- expand_grid(x=x,z=z)
    
    blocks <- get_subchunk_blocks(db, g$x, g$z, dimension, subchunk=0, names_only = TRUE)
    blocks <- purrr::compact(blocks)

    # find chunks with the most water at bedrock level
    water_count <- blocks %>% map_int(function(x) {
        b <- (x != "minecraft:bedrock")
        sum(b[,0:0,])
    })
    water_chunks <-  water_count[water_count > 0] %>% sort() %>% rev()

    # create an empty table if no results
    if(length(water_chunks) == 0L) {
        tbl <- tibble::tibble(
            x = integer(),
            z = integer(),
            size = integer()
        )
        return(tbl)
    }

    # create a table of the results
    pos <- chunk_pos(names(water_chunks))

    tibble::tibble(x = 16L*pos[,1],
                   z = 16L*pos[,2],
                   size = water_chunks)
}

