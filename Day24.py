import z3

def parseInput():
    input = open("inputs/day24.txt")
    positions = []
    velocities = []

    for line in input:
        line = line.strip()
        p, v = line.split(" @ ")
        positions.append(tuple(map(float, p.split(", "))))
        velocities.append(tuple(map(float, v.split(", "))))

    return positions, velocities

def solveEquation(positions, velocities):
    x = z3.Real('x')
    y = z3.Real('y')
    z = z3.Real('z')
    vx = z3.Real('vx')
    vy = z3.Real('vy')
    vz = z3.Real('vz')
    solver = z3.Solver()

    for i in range(len(positions)):
        xI, yI, zI = positions[i]
        vxI, vyI, vzI = velocities[i]
        tI = z3.Real(f"t_{i}")
        solver.add(xI + vxI * tI == x + vx * tI)
        solver.add(yI + vyI * tI == y + vy * tI)
        solver.add(zI + vzI * tI == z + vz * tI)
        solver.add(tI >= 0)

    solver.check()
    res = solver.model()
    return sum([res[x].as_long(), res[y].as_long(), res[z].as_long()])

positions, velocities = parseInput()
part2 = solveEquation(positions, velocities)

print(f"Part 1: Solved in Scala!")
print(f"Part 2: {part2}")
