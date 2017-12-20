# aoc2017 day 20

from collections import Counter


with open('input.txt', 'r') as f:
    data = f.readlines()

lines = [x.strip() for x in data if x]


def get_values(s):
    _, s = s.strip().split('<')
    s, _ = s.split('>')
    return map(int, s.split(','))


def tick(particle):
    _, p, v, a = particle
    v[0] += a[0]
    v[1] += a[1]
    v[2] += a[2]
    p[0] += v[0]
    p[1] += v[1]
    p[2] += v[2]


particles = [[i] + [get_values(x) for x in line.split(' ')] for i, line in enumerate(lines)]


#### Part 1
for i in range(500):
    map(tick, particles)

a = sorted(particles, key=lambda x: sum(map(abs, x[1])))
print a[0]
print a[-1]


#### Part 2
particles = [[i] + [get_values(x) for x in line.split(' ')] for i, line in enumerate(lines)]
for i in range(100):
    print 'start with => %d' % len(particles)
    map(tick, particles)
    particle_counts = Counter([tuple(x[1]) for x in particles])
    # remove collisions
    particles = [x for x in particles if particle_counts[tuple(x[1])] == 1]

