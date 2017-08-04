import fire
import json
import subprocess


def main(map, player):
    map = json.load(open(map))
    prv = None
    nxt = {
        'punter': 0,
        'punters': 1,
        'map': map
    }

    for _ in range(len(map['rivers'])):
        print(prv)
        print(nxt)
        print('--')

        p = subprocess.Popen(player, stdin=subprocess.PIPE, stdout=subprocess.PIPE)
        p.stdin.write('0:'.encode())
        p.stdin.write(json.dumps(nxt).encode())
        p.stdin.close()

        str = p.stdout.read().decode('utf-8')
        print(str)
        str = str.split(':', maxsplit=1)[1]
        prv = json.loads(str)

        nxt = {
            'move': {'moves': [{'claim': prv['claim']}]},
            'state': prv['state'],
        }

if __name__ == '__main__':
    fire.Fire(main)
