#!/usr/bin/env bash

# Parse VMD compatible .xyz format into
# gnuplottable file and generate plot

# Usage:
#  create_movie.sh input_file range delay frame_skip
#
# If input_file is -, input will be taken from stdin
# range is the x,y,z ranges that will be plotted
# delay is the time delay between frames
# frame_skip is the number of output frames to skip

# If no args, output usage
if [[ -z "$@" ]]; then
    echo "Usage: $(basename $0) input range [delay [frameskip]]"
    echo
    echo "         input: filename or \"-\" for stdin"
    echo
    echo "         range: Set the gnuplot ranges to [-range:range]"
    echo
    echo "         delay: The delay in seconds between frame outputs"
    echo
    echo "     frameskip: The number of frames to skip between outputs"
    echo
    exit
fi

input="$1"
shift

range="$1"
shift

delay="$1"
shift

frame_skip="$1"
shift

if [[ x"$delay" == x ]]; then
    delay=0
fi

if [[ x"$frame_skip" == x ]]; then
    frame_skip=1
fi


frame_count=0


#
# Process .xyz file into indexed splot format
#
# Expect format
#   num_particles
#   some comment
#   index1 x y z
#   index2 x y z
#   ...
#   num_particles
#   some comment
#   index1 x y z
#   index2 x y z
#   ...
#
# Process into format
#   splot - ...
#   index1 x y z
#   index2 x y z
#   ...
#   e
#
#
#   splot - ...
#   index1 x y z
#   index2 x y z
#   ...
#   e


#
# Create plotfile
#

# Bracket concatenates all output for piping to a single input
{
    # Define plot parameters
    echo "
    set xrange [-$range:$range]
    set yrange [-$range:$range]
    set zrange [-$range:$range]

    # set term gif animate optimize delay $delay
    # set output '$moviefile'
    "

    # Generate appropriate pipe for input data
    cat "$input"  |

    # Strip initial whitespace
    sed 's/^\ *//' |

    # Expect first line of sequence to contain only number of particles
    while read num_particles; do

        # Expect second line to contain comment
        read comment

        let frame_count++
        out_state=$((frame_count%frame_skip == 0))


        # If this is an output frame, read and output
        if [[ x"$out_state" == x1 ]]; then

            # Add some extra spacing between time steps
            echo
            echo

            # Output plot command
            echo "splot '-' using 1:2:3 \
                  with points palette pointsize 3 pointtype 7 \
                  title '$comment - Frame $frame_count'"

            #
            # Output plot data
            #

            # Loop over the given number of particles
            for i in $(seq 1 $num_particles); do
                # Read particle line
                read position

                # Output particle data
                echo $position
            done

            # Tell gnuplot data is finished
            echo e

            sleep $delay
        else
            # Read the given number of particles
            for i in $(seq 1 $num_particles); do read position; done
        fi
    done

# Output plotting data to gnuplot
} | gnuplot
