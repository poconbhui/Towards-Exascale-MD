# scripts/mpiexec.rb
#
# MODULE MpiExec
#
# This provides the run command which is used to run MPI jobs.
# It is expected to be used with the mpiexec environment variable
# set to either mpiexec or aprun.
#
module MpiExec


  # DEF self.mpiexec
  #
  # An accessor for the mpiexec variable with a default value
  # of either the mpiexec environment variable or "default"
  #
  def self.mpiexec
    @mpiexec ||= ENV["mpiexec"] || "default"
  end
  def self.mpiexec=(val)
    @mpiexec = val
  end


  # DEF run
  #
  # run accepts one string corresponding to a function to run and
  # a hash of options.
  #
  # Example usage:
  #     run("my_mpi_program", :mpi_procs => 4, :openmp_per_mpi => 1)
  #
  # The :mpi_procs option defaults to 1.
  # The :openmp_per_mpi option defaults to 1.
  #
  # The :mpi_procs option is used to set how many mpi processes the
  # mpiexec function should spawn.
  #
  # The :openmp_per_mpi option is used to set the OMP_NUM_THREADS
  # environment variable.
  #
  # Extra options specified will be passed unaltered to the underlying
  # mpiexec functions defined here.
  #
  def run(*args)
    # Sanitize options
    options = args.last.is_a?(Hash) ? args.pop : {}

    options[:cores] ||= 1
    options[:cores] = Integer(options[:cores])
    options[:mpi_procs] ||= options[:cores]
    options[:mpi_procs] = Integer(options[:mpi_procs])
    options[:openmp_per_mpi] ||= 1
    options[:openmp_per_mpi] = Integer(options[:openmp_per_mpi])

    args << options

    # Get appropriate mpiexec command
    case MpiExec.mpiexec
      when "aprun"
        shell_command = aprun(*args)
      else
        shell_command = mpiexec(*args)
    end

    # Execute command
    puts "#{shell_command}"
    puts `#{shell_command}`
  end


  # DEF aprun
  #
  # This function accepts a program to run and some configuration options
  # and returns a string that can be executed in a shell to run an
  # appropriate aprun command.
  #
  # This is aimed at HECToR Phase 3. As such, hector_max_cores_per_node is
  # set to 32. This value is used to determine an appropriate number of
  # MPI processes per node to request from aprun.
  #
  # This function takes both mpi_procs and openmp_per_mpi into account
  # then determining the number of MPI processes per node to allocate.
  # Where mpi_procs*openmp_per_mpi is greater than the number of cores
  # on a node, it divides the number of cores by the number of requested
  # openmp threads, and requests that many mpi processes per node.
  #
  # The number of MPI processes per node may be requested explicitly using
  # the :procs_per_node option.
  #
  def aprun(program, options)
    hector_max_cores_per_node = 32

    cores = options[:cores]
    mpi_procs = options[:mpi_procs]
    openmp_per_mpi = options[:openmp_per_mpi]

    procs_per_node = options[:procs_per_node] || mpi_procs
    procs_per_node = Integer(procs_per_node)
    if procs_per_node*openmp_per_mpi > hector_max_cores_per_node
        procs_per_node = (hector_max_cores_per_node/openmp_per_mpi).floor
    end

    "OMP_NUM_THREADS=#{openmp_per_mpi}
        time aprun -n #{mpi_procs} -N #{procs_per_node} -d #{openmp_per_mpi} \
        #{program}"
  end


  # DEF mpiexec
  #
  # This function accepts a program to run and some configuration options
  # and returns a string that can be executed in a shell to run an
  # appropriate mpiexec command.
  #
  # This is a very simple wrapper around mpiexec.
  #
  # It makes no assumptions about the underlying architecture
  # and simply sets the number of MPI processes to spawn to
  # the number requested through the :mpi_procs options and
  # sets the OMP_NUM_THREADS environment variable to the value
  # requested by :openmp_per_mpi option.
  #
  def mpiexec(program, options)
    "OMP_NUM_THREADS=#{options[:openmp_per_mpi]} \
        time mpiexec -n #{options[:mpi_procs]} #{program}"
  end

end
