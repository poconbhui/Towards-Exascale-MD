run "particle_type.test", :cores => 4

run "grav_force.test"
run "integration.test"
run "lj_force.test"

run "serial_distribution_type.test", :cores => 4
run "replicated_distribution_type.test", :cores => 4
run "domain_distribution_type.test", :cores => 4
run "systolic_distribution_type.test", :cores => 4
