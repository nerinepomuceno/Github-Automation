# pip install tensorflow keras numpy skimage matplotlib

##import keras
import numpy as np
import skimage
import matplotlib
# from turtle import *
import turtle
import random

# forward(100)
# shape('turtle')

# from turtle import *

# shape('turtle')
# def square():
#    for i in range(4):
#        forward(100)
#        right(90)
# square()


# def triangle(sidelength=100):
#    for i in range(3):
#        forward(sidelength)
#        right(60)
#        square()
#        triangle()
# triangle()


player_one = turtle.Turtle()
player_one.color("green")
player_one.shape("turtle")

player_two = turtle.Turtle()
player_two.color("blue")
player_two.shape("turtle")

player_one.penup()
player_two.penup()
player_one.goto(-100, -100)
player_two.goto(100, 100)

player_one.pendown()
player_two.pendown()

i = 1
previous_distance = player_one.distance(player_two)
p1_steps = random.choice(range(0, 50))
p1_angle = random.choice(range(-90, 90))
p2_steps = random.choice(range(0, 50))
p2_angle = random.choice(range(-90, 90))

while True:
    # random.normalvariate(0, 1)
    # forward(random.normalvariate(100, 100))

    current_distance = player_one.distance(player_two)

    print(i, current_distance)

    if i != 1:
        if current_distance > previous_distance:
            #player_one.backward(p1_steps)
            #player_one.left(p1_angle)
            #player_two.forward(p2_steps)
            #player_two.left(-p2_angle)
            next
        else:
            player_one.right(p1_angle)
            player_one.forward(p1_steps)

            player_two.left(p2_angle)
            player_two.backward(p2_steps)

    else:
        player_one.right(p1_angle)
        player_one.forward(p1_steps)

        player_two.left(p2_angle)
        player_two.backward(p2_steps)

    previous_distance = current_distance
    if player_one.distance(player_two) == 0:
        break
    i = i + 1
    p1_steps = random.choice(range(0, 50))
    p1_angle = random.choice(range(-90, 90))
    p2_steps = random.choice(range(0, 50))
    p2_angle = random.choice(range(-90, 90))

    # right(random.normalvariate(180, 100))

    # random.choice(range(0,100))














