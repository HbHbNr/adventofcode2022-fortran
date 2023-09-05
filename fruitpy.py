from FRUIT import *

test_modules = ['src/day02a_test.f90']
drivername = 'day02a_test_driver'
driver = f'src/{drivername}.f90'
output_dir = 'bin'
run_command = f'./{drivername}'
build_command = f'make {output_dir}/{drivername}'

suite = test_suite(test_modules)
suite.build_run(driver, build_command=build_command, run_command=run_command, output_dir=output_dir)
suite.summary()
# print(suite.output)
