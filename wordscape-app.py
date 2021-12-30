from typing import List, Set
from itertools import permutations


DICTIONARY_PATH = "/usr/share/dict/american-english"


def load_dictionary(path: str) -> Set[str]:
	with open(path, 'r') as f:
		return set(filter(lambda x: x.isalpha(),
			map(lambda x: x.strip().lower(), f.readlines())
			)
		) 


def perms(letters: List[str]) -> List[str]:
	output = []
	for i in range(len(letters), 2, -1):
		output.extend(
			sorted({''.join(p) for p in permutations(letters, i)})
		)
	
	return output


def actual_words(word_perms: List[str], dictionary: Set[str]) -> List[str]:
	return list(filter(lambda x: x in dictionary, word_perms))


def get_solution(characters: List[str], dictionary: Set[str]) -> List[str]:
	return actual_words(perms(characters), dictionary)


def main():
	dictionary = load_dictionary(DICTIONARY_PATH)
	if len(dictionary) == 0:
		print("Could not properly load dictionary")
		return 

	letters = input("Please type the letters involved with spaces in between each one: \n")
	chars = letters.strip().lower().split(" ")
	if len(chars) < 3:
		print("Invalid input, need at least three characters")
		return
	
	possible_solutions = get_solution(chars, dictionary)
	header = f"There are {len(possible_solutions)} possible solutions: "
	print("="* len(header))
	print(header)
	for word in possible_solutions:
		print(word)
	

if __name__ == "__main__":
    main()
