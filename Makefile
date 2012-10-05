all:
	cd src
	erl -make

run: all
	erl

datasets:
	cd data
	python ../bin/formatter.py car
	python ../bin/formatter.py car
clean:
	rm src/*.beam
	rm src/*.erl~