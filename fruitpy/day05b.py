from FRUIT import test_suite

day = 'day05b'

test_modules = [f'src/{day}_test.f90']
drivername = f'{day}_test_driver'
driver = f'src/{drivername}.f90'
output_dir = 'bin'
run_command = f'./{drivername}'
build_command = f'make {output_dir}/{drivername}'

suite = test_suite(test_modules)
suite.build_run(driver, build_command=build_command, run_command=run_command, output_dir=output_dir)
suite.summary()
