from FRUIT import test_suite

base = 'class_charstack'

test_modules = [f'src/{base}_test.f90']
drivername = f'{base}_test_driver'
driver = f'src/{drivername}.f90'
output_dir = 'bin'
run_command = f'./{drivername}'
build_command = f'make {output_dir}/{drivername}'

suite = test_suite(test_modules)
suite.build_run(driver, build_command=build_command, run_command=run_command, output_dir=output_dir)
suite.summary()