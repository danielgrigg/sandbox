from distutils.core import setup

setup(
    name='TyrionsCock',
    version='0.1.0',
    author='Danny G',
    author_email='mail@danielgrigg.com',
    packages=['tyrionscock', 'tyrionscock.test'],
    scripts=['bin/stowe-cock.py', 'bin/wash-cock.py'],
    url='http://pypy.python.org/pypy/TyrionsCock/',
    licence='LICENCE.txt',
    description='Useful cock-related stuff.',
    long_description=open('README.txt').read(),
    install_requires=[
      ],
    )

