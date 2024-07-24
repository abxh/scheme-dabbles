#!/usr/bin/env python3

def make_person(name, age):
    internal_name = name;
    internal_age = age;

    def make_person_internal(action, *args):
        nonlocal internal_name;
        nonlocal internal_age;

        match action:
            case "name":
                return internal_name
            case "age":
                return internal_age
            case "set-name":
                internal_name = args[0]
            case "set-age":
                internal_age = args[0]

    return make_person_internal


jack = make_person("Jack", 22)

print(jack("name"))
jack("set-name", "Mark")
jack("set-age", 24)
print(jack("name"), "is", jack("age"), "years old")

js = make_person("Marky", 23)

print(jack("name"))
print(js("name"))
