import ply.lex as lex
import Queue

# Result stack for maintaining the existing results
result_stack = set()

# This list is to store the standardized cnf_list
final_cnf_list = []

# This dictionary gives the (predicate) to (line, Predicate number in line) mapping
predicate_Line_Mapping = dict()

# For Standardizing the variables
count = 0

# This is for not forgetting any variable that is standardized
existing_variables = set()

# To get Every line to a format required for processing
every_line_made_as_list = dict()

# List Maintained For Resolution
list_for_resolution = []

# Global Line number
global_line_number = 1


'''
########################################################################################################################
# Step 1 : Tokenize the input
########################################################################################################################
'''
# List of token names. This is always required
tokens = (
  'PREDICATE',
  'LPAR',
  'RPAR',
  'IMPLICATION',
  'AND',
  'OR',
  'NOT'
)

# Regular expression rules for simple tokens
t_LPAR = r'\('
t_RPAR = r'\)'
t_IMPLICATION = r'\=>'
t_AND = r'\&'
t_OR = r'\|'
t_NOT = r'\~'

# A regular expression rule with some action code
#r'[A-Za-z]*[/(][a-zA-Z0-9,]*[/)]'
def t_PREDICATE(t):
    r'[A-Za-z]+\(.*?\)'
    return t


# Define a rule so we can track line numbers
def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)

# A string containing ignored characters (spaces and tabs)
t_ignore = ' \t'


# Error handling rule
def t_error(t):
    print("Illegal character '%s'" % t.value[0])
    t.lexer.skip(1)

# Build the lexer
lexer = lex.lex()


'''
########################################################################################################################
# Created the stack data structure
########################################################################################################################
'''


class Stack:
    def __init__(self):
        self.items = []

    def isEmpty(self):
        return self.items == []

    def push(self, item):
        self.items.append(item)

    def pop(self):
        return self.items.pop()

    def peek(self):
        return self.items[len(self.items) - 1]

    def size(self):
        return len(self.items)

    def getlist(self):
        return self.items

    def clear(self):
        del self.items[:]


def start_implication_removal(stack1, stack2):

    close_bracket_count = 0
    open_bracket_count = 0

    while not stack1.isEmpty():

        # if brackets simply increment the bracket count and push into stack2
        current_variable = stack1.pop()
        if current_variable == ")":
            close_bracket_count += 1
        elif current_variable == "(":
            open_bracket_count += 1

        # push all the elements from stack 2 to stack 1 by removing the negation symbols if any
        if open_bracket_count > close_bracket_count:
            temp = ""
            if current_variable == "(":
                stack1.push(current_variable)
            while not stack2.isEmpty():
                stack_2_temp = stack2.pop()
                if temp == "~":
                    if stack_2_temp[0] == temp:
                        stack1.pop()
                    else:
                        stack1.push(stack_2_temp)
                else:
                    stack1.push(stack_2_temp)
                temp = stack_2_temp
            break

        # negate this term and push to stack 2
        if close_bracket_count == open_bracket_count:
            if current_variable == "|":
                stack2.push("&")
            elif current_variable == "&":
                stack2.push("|")
                # This is a special case in which we push ~ itself
            elif current_variable == "~":
                stack2.push("~")
            else:
                stack2.push(current_variable)
                stack2.push("~")

        # push as it is to the stack
        if close_bracket_count > open_bracket_count:
            stack2.push(current_variable)

    temp = ""
    while not stack2.isEmpty():
        # do it
        stack_2_temp = stack2.pop()
        if temp == "~":
            if stack_2_temp[0] == temp:
                stack1.pop()
            else:
                stack1.push(stack_2_temp)
        else:
            stack1.push(stack_2_temp)
        temp = stack_2_temp
    return stack1

'''
########################################################################################################################
Now as the implication is removed, lets move the ~ inwards
1. If we encounter ~( then if  ~ is propagated by previous bracket, it cancels of.
2. If we encounter ~( then if ~ is not propagated from previous bracket, it propagates through.
3. If we encounter ( then if  ~ is propagated by previous bracket, it propagates through.
4. If we encounter ( then if ~ is not propagated from previous bracket, it cancels of.

Lets keep a list for implementation where each element in the list tells if the ~ is propagated or not by
an open bracket, increment the count whenever we encounter a open bracket. Decrement the count whenever we encounter
a closed bracket.

We have to modify the list as and when we decrement the count. Count =2, encountered a closed bracket, remove 2nd
element in the list and decrement the count
########################################################################################################################
'''


def move_not_inside(stack1, stack2):
    # Moving the not inwards
    open_bracket_list = []
    open_bracket_count = 0

    while not stack1.isEmpty():
        stack2.push(stack1.pop())
        # print(stack2.peek())

    previous_element = ""

    while not stack2.isEmpty():
        current_element = stack2.pop()
        if current_element == "(":
            open_bracket_count += 1
            if previous_element == "~":
                if stack1.peek() == "~":
                    stack1.pop()
                if open_bracket_count - 2 > 0:
                    if open_bracket_list[open_bracket_count - 2] == "~":
                        open_bracket_list.append("N")
                    else:
                        open_bracket_list.append("~")
                else:
                    open_bracket_list.append("~")
            else:
                if open_bracket_count - 2 > 0:
                    open_bracket_list.append(open_bracket_list[open_bracket_count - 2])
                else:
                    open_bracket_list.append("N")
            stack1.push(current_element)
            previous_element = current_element
        elif current_element == ")":
            open_bracket_count -= 1
            del open_bracket_list[open_bracket_count]
            stack1.push(current_element)
            previous_element = current_element
        else:
            if open_bracket_count == 0:
                stack1.push(current_element)
                previous_element = current_element
            else:
                propagate = open_bracket_list[open_bracket_count - 1]
                if propagate == "N":
                    stack1.push(current_element)
                else:
                    if current_element != "~":
                        if previous_element == "~":
                            stack1.push(current_element)
                        else:
                            if current_element == "&":
                                stack1.push("|")
                            elif current_element == "|":
                                stack1.push("&")
                            else:
                                stack1.push("~")
                                stack1.push(current_element)
                previous_element = current_element

    return stack1


def get_priority(expression):
    if expression == "(":
        return 4
    if expression == "&":
        return 3
    if expression == "|":
        return 2


def to_postfix(stack2):
    # create the instance
    q = Queue.LifoQueue()
    s = Stack()
    for t in stack2.getlist():
        if not t == "|" and not t == "(" and not t == "&" and not t == ")":
            q.put(t)
        elif t == "|" or t == "&":
            if s.isEmpty():
                s.push(t)
            elif not s.isEmpty() and get_priority(s.peek()) >= get_priority(t):
                s.push(t)
            else:
                while not s.isEmpty() and get_priority(s.peek()) > get_priority(t):
                    q.put(s.pop())
        elif t == "(":
            s.push(t)
        elif t == ")":
            while not s.isEmpty() and not s.peek() == "(":
                q.put(s.pop())
            if not s.isEmpty():
                s.pop()
    while not s.isEmpty():
            q.put(s.pop())

    return q


def distribute_and_over_or(q):
    global_list = []
    for elem in list(q.queue):
        if elem == "&":
            # print(elem)
            list_temp = global_list[-1]
            del global_list[-1]
            for t in list_temp:
                global_list[-1].append(t)
            # take the last list from the global list and put each of its element into the list before it
            # print("from &")
            # print(global_list)
        elif elem == "|":
            # print(elem)
            # take the last list from the global list and combine each of this list's items to the each item of
            #  list before it by using a | operator in between
            list_temp = global_list[-1]
            del global_list[-1]
            list_temp_left = global_list[-1]
            del global_list[-1]
            # global_list.append([""])
            global_list.append([])
            for t in list_temp:
                for i in list_temp_left:
                    global_list[len(global_list) - 1].append(i + '|' + t)
            # print("from |")
            # print(global_list)
        else:
            global_list.append([elem])
            # print("from else")
            # print(global_list)
    return global_list


def standardize_variables(list_passed):
    global count
    global existing_variables
    temp_list_local = []
    for temp in list_passed:
        all_relation = temp.split("|")
        line_dict = dict()
        temp_sentence = ""
        for each_relation in all_relation:
            dont_know = each_relation.split("(")
            temp_sentence = temp_sentence + dont_know[0].strip() + "("
            all_var_const = dont_know[1].split(",")
            for i in all_var_const:
                i = i.strip()
                j = ""
                if ")" in i[-1]:
                    i = i[:-1]
                    j = ")"
                if not i[0].isupper():
                    if i in existing_variables:
                        if i in line_dict.keys():
                            i = line_dict[i]
                        else:
                            temp_change = i + str(count)
                            line_dict[i] = temp_change
                            i = temp_change
                            count += 1
                            existing_variables.add(i)
                    else:
                        existing_variables.add(i)
                        line_dict[i] = i
                j = "," if j == "" else ")"
                temp_sentence = temp_sentence + i + j
            temp_sentence += "|"
        temp_list_local.append(temp_sentence[0:-1])

    return temp_list_local


def fill_map(list_cnf):
    line_number = 0
    for temp in list_cnf:
        arr = temp.split('|')
        line_number += 1
        predicate_number = 1
        for i in range(0, len(arr)):
            split1 = arr[i].split('(')
            for j in range(0, len(split1)):
                if ")" not in split1[j]:
                    if split1[j] in predicate_Line_Mapping.keys():
                        predicate_Line_Mapping[split1[j]].append((line_number, predicate_number))
                    else:
                        predicate_Line_Mapping[split1[j]] = [(line_number, predicate_number)]
                    predicate_number += 1


def sentence_to_cnf(input_data):
    global final_cnf_list
    del final_cnf_list[:]
    cnf_list = []
    for sentence in input_data:
        # List for storing all tokens after tokenizing
        # print sentence
        token_list = []
        stack1 = Stack()
        stack2 = Stack()
        lexer.input(sentence)
        # Tokenize
        while True:
            tok = lexer.token()
            if not tok:
                break  # No more input
            token_list.append(tok.value)
            # print(tok.value)

        for t in token_list:
            if t != "=>":
                stack1.push(t)
            else:
                stack2.clear()
                stack2.push("|")
                stack1 = start_implication_removal(stack1, stack2)
        stack2.clear()
        stack1 = move_not_inside(stack1, stack2)
        stack2.clear()
        last_var = ""
        for t in stack1.getlist():
            if not t == "~":
                if last_var == "~":
                    stack2.push("~"+t)
                else:
                    stack2.push(t)
            last_var = t
        # Convert to postfix notation, then we can convert to CNF
        stack1.clear()
        q = to_postfix(stack2)
        global_list = distribute_and_over_or(q)
        temp_list = global_list[0]
        for t in temp_list:
            cnf_list.append(t)

    final_cnf_list = standardize_variables(cnf_list)
    fill_map(final_cnf_list)


def predicate_line_mapping_function(relation_sentence):
    # Given a sentence and a line number we have to map each and every predicate to the
    # (line_number, position of predicate in this line)
    # Usage :
    # predicate_line_mapping_function(string)
    global_line_number += 1
    arr = relation_sentence.split('|')
    predicate_number = 1
    for i in range(0, len(arr)):
        split1 = arr[i].split('(')
        for j in range(0, len(split1)):
            if ")" not in split1[j]:
                if split1[j] in predicate_Line_Mapping.keys():
                    predicate_Line_Mapping[split1[j]].append((global_line_number, predicate_number))
                else:
                    predicate_Line_Mapping[split1[j]] = [(global_line_number, predicate_number)]
                predicate_number += 1


def get_predicate_name(each_relation):
    temp = each_relation.split("(")
    return temp[0]


def get_complete_sentence(var):
    global list_for_resolution
    return list_for_resolution[var-1]


def get_specific_relation(complete_sentence, position):
    complete_sentence_array = complete_sentence.split("|")
    return complete_sentence_array[position-1]



def UNIFY_VAR(var, x, theta):
    if var in theta.keys():
        return UNIFY(theta[var], x, theta)
    elif x in theta.keys():
        return UNIFY(var, theta[x], theta)
    else:
        if var[0].islower():
            theta[var] = x
        elif x[0].islower():
            theta[x] = var
        else:
            theta[var] = x
    return theta


def UNIFY(X,Y,theta):
    if theta == False:
        return False
    elif X == Y:
        return theta
    elif type(X) is not list and X[0].islower():
        return UNIFY_VAR(X,Y,theta)
    elif type(Y) is not list and Y[0].islower():
        return UNIFY_VAR(Y, X, theta)
    elif type(X) is list and type(Y) is list:
        return UNIFY(X[1:], Y[1:], UNIFY(X[0], Y[0], theta))
    else:
        return False
'''
def is_unification_possible(result_tuple, kb_tuple):

    unifier = dict()

    result_array = result_tuple.split("(")
    kb_array = kb_tuple.split("(")

    result_array_values = result_array[1]
    kb_array_values = kb_array[1]

    result_array_tuple = result_array_values.split(",")
    kb_array_tuple = kb_array_values.split(",")

    for i in range(0, len(result_array_tuple)):
        if kb_array_tuple[i][-1] == ")":
            kb_array_tuple[i] = kb_array_tuple[i][:-1]
            result_array_tuple[i] = result_array_tuple[i][:-1]

        if kb_array_tuple[i] != result_array_tuple[i]:
            if kb_array_tuple[i][0].isupper() and result_array_tuple[i][0].isupper():
                return dict()
            elif kb_array_tuple[i][0].islower() and result_array_tuple[i][0].islower():
                if kb_array_tuple[i] not in unifier.keys():
                    unifier[kb_array_tuple[i]] = result_array_tuple[i]
            else:
                if kb_array_tuple[i][0].islower():
                    var = kb_array_tuple[i]
                    const = result_array_tuple[i]
                else:
                    const = kb_array_tuple[i]
                    var = result_array_tuple[i]
                if var in unifier.keys():
                    if unifier[var][0].isupper():
                        if unifier[var] != const:
                            return dict()
                unifier[var] = const
    if len(unifier) == 0:
        unifier["Game"] = "Game"
    # print "adarsh" + str(unifier)
    return unifier
'''

def get_remaining_sentence(tuple_to_remove, complete_sentence):
    complete_sentence_array = complete_sentence.split("|")
    complete_sentence_array.remove(tuple_to_remove)
    to_return_string = ""
    for temp in complete_sentence_array:
        to_return_string += temp + "|"
    if len(to_return_string) > 0:
        return to_return_string[:-1]
    return to_return_string


def check_if_value_has_to_change(t, substitutions):
    value = substitutions[t]
    while value in substitutions.keys():
        value = substitutions[value]
    return value


def get_combined_substituted_string(result_string, kb_string, substitutions):
    combined = ""
    if len(result_string) != 0 and len(kb_string) != 0:
        combined += result_string + "|" + kb_string
    elif len(kb_string) != 0:
        combined += kb_string
    elif len(result_string) != 0:
        combined += result_string
    else:
        return combined

    list1 = combined.split("|")

    main_string = ""
    for temp in list1:
        split_predicate = temp.split("(")
        main_string += split_predicate[0]
        main_string += "("
        split_variables = split_predicate[1].split(",")
        for t in split_variables:
            # print t
            append_check = ""
            if t[-1] == ")":
                append_check = ")"
                t = t[:-1]
            if t[0].islower():
                if t in substitutions.keys():
                    value_to_substitute = check_if_value_has_to_change(t, substitutions)
                    main_string += value_to_substitute
                else:
                    main_string += t
            else:
                    main_string += t
            if append_check == ")":
                main_string += append_check
                main_string += "|"
            else:
                main_string += ","
    return main_string[:-1]


def create_sorted_list(combined_string):
    # print "From create sorted list"
    # print combined_string
    temp_array = combined_string.split("|")
    temp_array.sort()
    # print temp_array
    return_string = ""
    for temp in temp_array:
        return_string += temp + "|"
    if len(return_string) > 0:
        return return_string[:-1]
    return return_string


def not_in_existing_results(to_be_put_in_result_set):
    if to_be_put_in_result_set in result_stack:
        return False
    return True


def remove_duplicates(combined_string):
    if len(combined_string) == 0:
        return combined_string
    sorted_list = create_sorted_list(combined_string)
    split_it = sorted_list.split("|")
    to_return_string = ""
    is_duplicate = ""
    for temp in split_it:
        if temp != is_duplicate:
            to_return_string += temp + "|"
        is_duplicate = temp
    return to_return_string[:-1]


'''
# while doing resolution we have to resolve with 1st list item of the result that contains a CNF sentence
# whenever a unification is possible, we have to add the old result into the KB. Whenever a unification is not possible
# with any of the relations in this sentence we have to go to the old result set and try to resolve other attributes in
# it.

Steps :
1. check if unification possible
2. If unification possible, then unify and put the old result in to the list_for_resolution
3. Parallel to it, put the old result into a stack, I will tell the use of this later
4. Before or after putting old result into the list_for_resolution make sure to call predicate_line_mapping_function
5. Recursively call the start_resolution function, until we find a empty set, we have to return false, whenever there
   is no unification possible with any of the sentences in the result set, we have to try a separate branch
   .....We might even return when we find that after resolution we have reached one of the answers in the result set
   then we have to return false.....

'''


def create_predicate_con_var_list(each_relation):
    temp_list = []
    array = each_relation.split("(")
    temp_list.append(array[0])
    array1 = array[1].split(",")
    for temp in array1:
        if temp[-1] == ")":
            var = temp[:-1]
        else:
            var = temp
        temp_list.append(var)
    return temp_list


def get_list_of_literals(my_string):
    temp_list = []
    temp_array = my_string.split("(")
    temp_string = temp_array[1]
    temp_string = temp_string[:-1]
    all_var_array = temp_string.split(",")
    for each_var in all_var_array:
        temp_list.append(each_var)
    return temp_list


def start_resolution(result_string, branch_line_predicate_used):

    global global_line_number
    # print "global line number " + str(global_line_number)

    global result_stack

    if len(result_string) == 0:
        return True
    # print "result string" + result_string

    # break the initial result string by OR
    result_string_array = result_string.split("|")

    for result_tuple in result_string_array:
        # get all the matching predicate line and position from this
        predicate_name = get_predicate_name(result_tuple)
        if predicate_name[0] == "~":
            predicate_name = predicate_name[1:]
        else:
            predicate_name = "~" + predicate_name

        if predicate_name in predicate_Line_Mapping.keys():
            line_relation_no = predicate_Line_Mapping[predicate_name]
        else:
            line_relation_no = []

        for may_unify_string in line_relation_no:
            # print "TO BE UNIFIED STRING adarsh" + result_tuple + " Predicate name" + predicate_name
            complete_sentence = get_complete_sentence(may_unify_string[0])
            # print "Complete sentence adarsh" + complete_sentence
            specific_relation_in_kb = get_specific_relation(complete_sentence, may_unify_string[1])
            # print "specific relation" + specific_relation_in_kb

            #substitutions = is_unification_possible(result_tuple, specific_relation_in_kb)

            # check unification specific List
            #print "Predicate 1 : " + result_tuple
            X = get_list_of_literals(result_tuple)
            # print X
            #print "Predicate 2 : " + specific_relation_in_kb
            Y = get_list_of_literals(specific_relation_in_kb)
            # print Y

            substitutions = UNIFY(X, Y, dict())

            if type(substitutions) is not dict:
                substitutions = dict()
                continue
            else:
                if len(substitutions) == 0:
                    substitutions["Game"] = "Game"

            if len(substitutions) != 0:
                remaining_result = get_remaining_sentence(result_tuple, result_string)
                # print "remaining_result = " + remaining_result
                remaining_kb = get_remaining_sentence(specific_relation_in_kb, complete_sentence)
                # print "remaining_kb = " + remaining_kb
                combined_string = get_combined_substituted_string(remaining_result, remaining_kb, substitutions)
                # print "combined_string = " + combined_string
                duplicate_removed_combined_string = remove_duplicates(combined_string)
                if len(duplicate_removed_combined_string) == 0:
                    return True
                # to_be_put_in_result_set = create_sorted_list(duplicate_removed_combined_string)
                # print "to_be_put_in_result_set = " + duplicate_removed_combined_string
                if not_in_existing_results(duplicate_removed_combined_string):

                    # start
                    temp_var_list = create_predicate_con_var_list(result_tuple)
                    # print "temp_list_adarsh"
                    # print temp_var_list
                    a = list(may_unify_string)
                    for element in temp_var_list:
                        a.append(element)
                    a = tuple(a)
                    # print a
                    # end

                    if a not in branch_line_predicate_used:
                        result_stack.add(duplicate_removed_combined_string)
                        branch_line_predicate_used.add(a)
                        is_true = start_resolution(duplicate_removed_combined_string, branch_line_predicate_used)
                        branch_line_predicate_used.remove(a)
                        if is_true:
                            return True
    return False


if __name__ == "__main__":
    inputFile = open('input.txt', 'r')
    outputFile = open('output.txt', 'w')
    no_of_test_cases = int(inputFile.readline())
    queries = []
    for each_case in range(no_of_test_cases):
        query = inputFile.readline()
        query = query.strip()
        query = query.replace(' ', '')
        queries.append(query)

    no_of_kb_lines = int(inputFile.readline())
    kb = []
    for each_case in range(no_of_kb_lines):
        sentence_read = inputFile.readline()
        sentence_read = sentence_read.replace(' ', '')
        kb.append(sentence_read)

    # every_line_made_as_list = every_line_made_to_list(final_cnf_list)
    number = 0

    for each_query in queries:
        if each_query[0] == "(":
            each_query = each_query[1:-1]
        if each_query[0] == "~":
            each_query = each_query[1:]
        else:
            each_query = "~" + each_query

        result_stack.clear()
        count = 0
        predicate_Line_Mapping.clear()
        existing_variables.clear()
        # Convert Each sentence to CNF
        new_kb = kb[:]
        new_kb.append(each_query)
        sentence_to_cnf(new_kb)
        del new_kb
        global_line_number = len(final_cnf_list)
        initial_result_set_list = standardize_variables([each_query])
        list_for_resolution = final_cnf_list[:]
        # print list_for_resolution
        # Start of resolution, we pass the result set along with it
        result = start_resolution(initial_result_set_list[0], set())
        print result

        if result:
            if number != 0:
                outputFile.writelines('\n')
            outputFile.writelines("TRUE")
            number += 1
        else:
            if number != 0:
                outputFile.writelines('\n')
            outputFile.writelines("FALSE")
            number += 1

