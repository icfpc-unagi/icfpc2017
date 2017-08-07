using System;
using System.Collections.Generic;
using System.Runtime.Serialization;
using System.IO;
using System.Text;
using System.Runtime.Serialization.Json;


[DataContract]
public class Site
{
    [DataMember(Name = "id")]
    public int id { get; set; }
}

[DataContract]
public class River
{
    [DataMember(Name = "source")]
    public int source { get; set; }
    [DataMember(Name = "target")]
    public int target { get; set; }
}

[DataContract]
public class Map
{
    [DataMember(Name = "sites")]
    public List<Site> sites { get; set; }
    [DataMember(Name = "rivers")]
    public List<River> rivers { get; set; }
    [DataMember(Name = "mines")]
    public List<int> mines { get; set; }
}

[DataContract]
public class SetUpInput
{
    [DataMember(Name = "punter")]
    public int punter { get; set; }
    [DataMember(Name = "punters")]
    public int punters { get; set; }
    [DataMember(Name = "map")]
    public Map map { get; set; }
    [DataMember(Name = "settings")]
    public Settings settings { get; set; }
}


[DataContract]
public class Settings
{
    [DataMember]
    public bool futures;
    [DataMember]
    public bool options;
    [DataMember]
    public bool splurges;
}


[DataContract]
public class SetUpOutput
{
    [DataMember(Name = "ready")]
    public int ready { get; set; }
    [DataMember(Name = "state")]
    public string state { get; set; }
    public SetUpOutput() { }
}

[DataContract]
public class Claim
{
    [DataMember(Name = "punter")]
    public int punter { get; set; }
    [DataMember(Name = "source")]
    public int source { get; set; }
    [DataMember(Name = "target")]
    public int target { get; set; }

    public Claim(int p, int s, int t)
    {
        punter = p;
        source = s;
        target = t;
    }
}

[DataContract]
public class Splurge
{
    [DataMember(Name = "punter")]
    public int punter { get; set; }
    [DataMember(Name = "route")]
    public List<int> route { get; set; }

    public Splurge(int a, List<int> b)
    {
        punter = a;
        route = b;
    }
}


[DataContract]
public class Option
{
    [DataMember(Name = "punter")]
    public int punter { get; set; }
    [DataMember(Name = "source")]
    public int source { get; set; }
    [DataMember(Name = "target")]
    public int target { get; set; }

    public Option(int p, int s, int t)
    {
        punter = p;
        source = s;
        target = t;
    }
}


[DataContract]
public class Move2
{
    [DataMember(Name = "claim")]
    public Claim claim { get; set; }
    [DataMember(Name = "splurge")]
    public Splurge splurge { get; set; }
    [DataMember(Name = "option")]
    public Option option { get; set; }
}

[DataContract]
public class Move
{
    [DataMember(Name = "moves")]
    public List<Move2> moves { get; set; }
}

[DataContract]
public class MoveInput
{
    [DataMember(Name = "move")]
    public Move move { get; set; }
    [DataMember(Name = "state")]
    public string state { get; set; }
}

[DataContract]
public class MoveOutput
{
    [DataMember(Name = "claim")]
    public Claim claim { get; set; }
    [DataMember(Name = "state")]
    public string state { get; set; }
}

[DataContract]
public class MoveOutputOption
{
    [DataMember(Name = "option")]
    public Option option { get; set; }
    [DataMember(Name = "state")]
    public string state { get; set; }
}



public class ProbNode : IComparable<ProbNode>
{
    public double score;
    public int id;

    public ProbNode(int id, double score)
    {
        this.score = score;
        this.id = id;
    }

    public int CompareTo(ProbNode other)
    {
        return -score.CompareTo(other.score);
    }
}

class Rank : IComparable<Rank>
{
    public double score;
    public int a, b;
    public Rank(double score, int a, int b)
    {
        this.score = score;
        this.a = a;
        this.b = b;
    }

    public int CompareTo(Rank other)
    {
        return score.CompareTo(other.score);
    }
}


public class ProbNode2 : IComparable<ProbNode2>
{
    public double score;
    public int id;

    public ProbNode2(int id, double score)
    {
        this.score = score;
        this.id = id;
    }

    public int CompareTo(ProbNode2 other)
    {
        return score.CompareTo(other.score);
    }
}

class MyState
{

    int N;
    int punters;
    int punter;
    int AllScore;
    Dictionary<int, int> dic = new Dictionary<int, int>();
    List<int>[] es;
    int[,,] id;
    int options;

    int[] mines;
    long[] mark;
    long[] mark2;

    public MyState() { }

    public MyState(SetUpInput inp)
    {
        N = inp.map.sites.Count;
        punters = inp.punters;
        punter = inp.punter;
        mines = inp.map.mines.ToArray();
        AllScore = 0;

        if (inp.settings != null)
        {
            if (inp.settings.options == true)
            {
                options = mines.Length;
                //options = 0;
            }
        }

        List<int> a = new List<int>();
        List<int> b = new List<int>();
        foreach (var r in inp.map.rivers)
        {
            a.Add(r.source);
            b.Add(r.target);
            N = Math.Max(r.source + 1, N);
            N = Math.Max(r.target + 1, N);
        }



        id = new int[N, N, 2];
        es = new List<int>[N];
        for (int i = 0; i < N; i++)
        {
            es[i] = new List<int>();
        }
        for (int i = 0; i < a.Count; i++)
        {
            es[a[i]].Add(b[i]);
            es[b[i]].Add(a[i]);
        }
    }

    public MyState(MoveInput move)
    {
        stateToMyState(move.state);
        foreach (var m in move.move.moves)
        {
            moveUpdate(m);
        }
    }

    public void moveUpdate(Move2 m)
    {
        if (m == null) return;
        if (m.claim != null)
        {
            Claim c = m.claim;
            int p = c.punter;
            int s = c.source;
            int t = c.target;
            id[s, t, 0] = id[t, s, 0] = p + 1;
        }
        if (m.option != null)
        {
            Option o = m.option;
            int p = o.punter;
            int s = o.source;
            int t = o.target;
            id[s, t, 1] = id[t, s, 1] = p + 1;
        }
        if (m.splurge != null)
        {
            Splurge s = m.splurge;
            int p = s.punter;
            for (int i = 0; i < s.route.Count - 1; i++)
            {
                if (id[s.route[i], s.route[i + 1], 0] != 0)
                {
                    id[s.route[i], s.route[i + 1], 1] = p + 1;
                    id[s.route[i + 1], s.route[i], 1] = p + 1;
                }
                else
                {
                    id[s.route[i], s.route[i + 1], 0] = p + 1;
                    id[s.route[i + 1], s.route[i], 0] = p + 1;
                }
            }
        }
    }


    UnionFind uni;
    int[][] dist;
    List<int>[] ID;
    List<int>[] ID2;

    double[][] prob;

    List<int> AID;
    List<int> BID;

    //double[,] addScore;
    //double[,] addAve;

    Dictionary<int, double> nowPoint;
    Dictionary<int, double> nowPoint2;
    Dictionary<int, double> randDist;
    int[] prePos;
    int[] preMyPos;
    double[] myDist;
    Random r;


    Dictionary<int, double> addScore;
    bool firstTime = false;
    int mineEdge = 0;
    bool[] isMine;

    public Move2 makeClaim()
    {
        Console.Error.WriteLine($"Think Start Time : {Myon.getTime()}");
        r = new Random();
        int p = punter;

        int s = 0;
        int t = 0;

        initStrategy();

        Console.Error.WriteLine($"Init End Time : {Myon.getTime()}");

        addScore = new Dictionary<int, double>();
        //addScore = new double[N, N];
        //addAve = new double[N, N];
        /*
        for (int i = 0; i < N; i++)
        {
            for (int j = 0; j < N; j++)
            {
                addScore[i, j] = -1;
            }
        }
        */


        for (int i = 0; i < N; i++)
        {
            if (prob[i] == null) continue;

            /*
            Console.Error.WriteLine(i);
            for (int j = 0; j < N; j++)
            {
                if(ID[j].Count != 0) Console.Error.Write($"{ID[j].Count}:{prob[i][j]:0.00} ");
            }
            Console.Error.WriteLine();
            */
        }

        List<Rank> LR = new List<Rank>();
        LR.Add(new Rank(-99999999, 0, 0));

        nowPoint = new Dictionary<int, double>();
        nowPoint2 = new Dictionary<int, double>();

        for (int i = 0; i < N; i++)
        {
            foreach (var j in es[i])
            {
                if (i > j) continue;
                if (id[i, j, 0] != 0 && (options == 0 || id[i, j, 1] != 0)) continue;
                double nowscore = 0;

                int ri = uni.root(i);
                int rj = uni.root(j);

                if (firstTime && (isMine[i] || isMine[j]) && punters * 2 > mineEdge)
                {
                    nowscore += 99999999;
                }

                if (ri != rj)
                {
                    //prob score
                    foreach (var a in AID)
                    {
                        foreach (var b in AID)
                        {
                            double maxA = Math.Max(prob[a][ri], prob[a][rj]);
                            double minA = Math.Min(prob[a][ri], prob[a][rj]);
                            double pA = maxA + (1 - maxA) * minA * 0.05;

                            double maxB = Math.Max(prob[b][ri], prob[b][rj]);
                            double minB = Math.Min(prob[b][ri], prob[b][rj]);
                            double pB = maxB + (1 - maxB) * minB * 0.05;

                            double newP = pA * pB;

                            double oldP = Math.Max(Math.Max(prob[a][b], prob[b][a]), Math.Max(prob[a][ri] * prob[b][ri], prob[a][rj] * prob[b][rj]));
                            //double oldP = Math.Max(prob[a][b], prob[b][a]);
                            if (newP < oldP) continue;

                            double nextP = Math.Max(newP - oldP, 0);

                            nowscore += getAddScore(a, b) * nextP;
                            nowscore += getAddScore(b, a) * nextP;

                            //if (ri == a) nowscore += addAve[a, b] * ID2[a].Count * nextP;
                            //if (rj == a) nowscore += addAve[a, b] * ID2[a].Count * nextP;
                            //if (ri == b) nowscore += addAve[b, a] * ID2[b].Count * nextP;
                            //if (rj == b) nowscore += addAve[b, a] * ID2[b].Count * nextP;


                        }
                    }

                    foreach (var a in AID)
                    {
                        nowscore += getAddScore(a, ri) * Math.Max(0, prob[a][rj] - prob[a][ri]);
                        nowscore += getAddScore(a, rj) * Math.Max(0, prob[a][ri] - prob[a][rj]);
                        //nowscore += getAddScore(a, rj) * prob[a][ri];
                    }

                    //Console.Error.WriteLine($"{i} {j} {nowscore}");

                }

                nowPoint[i * N + j] = nowscore;
            }
        }


        randDist = new Dictionary<int, double>();
        int MAXT = 10;
        double heavy = 50;
        for (int ttt = 0; ttt < MAXT; ttt++)
        {
            if (ttt != 0 && Myon.getTime() > 550)
            {
                MAXT = ttt;
                break;
            }
            foreach (var mine in AID)
            {
                for (int i = 0; i < N; i++)
                {
                    foreach (var j in es[i])
                    {
                        randDist[i * N + j] = -Math.Log(r.NextDouble());
                        if (id[i, j, 0] != 0) randDist[i * N + j] += 2;
                    }
                }

                myDist = new double[N];
                for (int i = 0; i < N; i++)
                {
                    myDist[i] = 99999999;
                }
                Heap<ProbNode2> h = new Heap<ProbNode2>();
                int firstR = uni.root(mine);
                h.push(new ProbNode2(firstR, 0));
                prePos = new int[N];
                preMyPos = new int[N];
                myDist[firstR] = 0;
                prePos[firstR] = -1;
                bool[] usedD = new bool[N];
                while (h.top != null)
                {
                    int now = h.pop().id;
                    if (usedD[now]) continue;
                    usedD[now] = true;
                    double nowC = myDist[now];
                    //Console.Error.WriteLine(now + " " + nowC);

                    foreach (var i in ID[now])
                    {
                        foreach (var j in es[i])
                        {
                            //if (id[i, j, 0] != 0) continue;
                            if (id[i, j, 0] != 0 && (options == 0 || id[i, j, 1] != 0)) continue;
                            int rj = uni.root(j);
                            int hash = Math.Min(i, j) * N + Math.Max(i, j);
                            double nextC = nowC + randDist[hash];
                            if (myDist[rj] - 1e-9 > nextC)
                            {
                                myDist[rj] = nextC;
                                preMyPos[rj] = j;
                                prePos[rj] = i;
                                h.push(new ProbNode2(rj, nextC));
                            }
                        }
                    }
                }
                int[] que = new int[N];
                foreach (var i in BID)
                {
                    if (prePos[i] != -1) que[uni.root(prePos[i])]++;
                }
                Queue<int> q = new Queue<int>();

                foreach (var i in BID)
                {
                    if (que[i] == 0) q.Enqueue(i);
                }
                double[] AllScore = new double[N];

                while (q.Count != 0)
                {
                    int now = q.Dequeue();
                    double PP = getAddScore(firstR, now) * prob[firstR][now];
                    AllScore[now] += PP;
                    if (prePos[now] != -1)
                    {
                        AllScore[now] -= PP;
                        int EI = prePos[now];
                        int EJ = preMyPos[now];
                        int RI = uni.root(EI);
                        int RJ = uni.root(EJ);
                        int hash = Math.Min(EI * N + EJ, EJ * N + EI);
                        if (!nowPoint.ContainsKey(hash)) nowPoint[hash] = 0;
                        if (!nowPoint2.ContainsKey(hash)) nowPoint2[hash] = 0;
                        nowPoint2[hash] += AllScore[now] * heavy;

                        AllScore[RI] += AllScore[now];
                        que[RI]--;
                        if (que[RI] == 0) q.Enqueue(RI);
                    }
                }
            }
        }

        List<Rank> preLR = new List<Rank>();
        foreach (var i in BID)
        {
            foreach (var j in es[i])
            {
                if (i > j) continue;
                if (id[i, j, 0] != 0 && (options <= 0 || id[i, j, 1] != 0)) continue;
                if (!nowPoint.ContainsKey(i * N + j)) nowPoint[i * N + j] = 0;
                preLR.Add(new Rank(nowPoint[i * N + j], i, j));

                if (nowPoint2.ContainsKey(i * N + j))
                {
                    double add = nowPoint2[i * N + j];
                    if (uni.size(i) != 1 || uni.size(j) != 1)
                    {
                        add /= two[bitCount(mark[i] | mark[j])];
                    }
                    if (isMine[uni.root(i)] || isMine[uni.root(j)]) add *= 1.7;
                    nowPoint[i * N + j] += add / MAXT;
                }
            }

        }



        Console.Error.WriteLine($"All Search Time : {Myon.getTime()}");

        double best = -99999999;
        for (int i = 0; i < N; i++)
        {
            foreach (var j in es[i])
            {
                if (i > j) continue;
                //if (id[i, j] != 0) continue;
                if (id[i, j, 0] != 0 && (options == 0 || id[i, j, 1] != 0)) continue;
                if (!nowPoint.ContainsKey(i * N + j)) nowPoint[i * N + j] = 0;
                LR.Add(new Rank(nowPoint[i * N + j], i, j));
            }
        }



        LR.Sort();
        LR.Reverse();
        preLR.Sort();
        preLR.Reverse();
        s = LR[0].a;
        t = LR[0].b;

        bool obst = Myon.getTime() <= 230;
        //bool obst = false;

        /*
        for (int tt = 0; tt < 140; tt++)
        {
            int a = LR[tt].a;
            int b = LR[tt].b;
            Console.Error.WriteLine(LR[tt].score + " " + isMine[a] + " " + isMine[b] + " " + nowPoint2[a * N + b]/MAXT);
        }
        */

        if (Myon.getTime() < 500)
        {
            double firstmyres = -1;
            Dictionary<int, bool> usedcheck = new Dictionary<int, bool>();
            for (int tt = 0; tt < LR.Count && Myon.getTime() <= 700; tt++)
            {
                //2
                for (int ttt = 0; ttt < Math.Min(2, options + 2) && Myon.getTime() <= 700; ttt++)
                {
                    int A = LR[tt].a;
                    int B = LR[tt].b;
                    if (ttt == 1)
                    {
                        if (preLR.Count >= ttt) continue;
                        A = preLR[tt].a;
                        B = preLR[tt].b;
                    }
                    if (usedcheck.ContainsKey(A * N + B)) continue;
                    usedcheck[A * N + B] = true;

                    int idP = 0;
                    if (id[A, B, 0] != 0 || id[B, A, 0] != 0)
                    {
                        idP = 1;
                    }
                    id[A, B, idP] = id[B, A, idP] = punter + 1;
                    initStrategy();
                    double exScore = 0;
                    if (nowPoint2.ContainsKey(A * N + B)) exScore += nowPoint2[A * N + B];
                    exScore /= 100000;

                    double myres = 0;
                    foreach (var i in mines)
                    {
                        for (int j = 0; j < N; j++)
                        {
                            myres += dist[i][j] * dist[i][j] * prob[uni.root(i)][uni.root(j)];
                        }
                    }
                    if (firstmyres == -1) firstmyres = myres;
                    exScore += myres;

                    if (firstTime && (isMine[A] || isMine[B]) && punters * 2 > mineEdge)
                    {
                        exScore += 99999999;
                    }

                    if (obst)
                    {
                        for (int k = 0; k < N; k++)
                        {
                            if (k != punter) continue;
                            if (Myon.getTime() > 730)
                            {
                                exScore = -999999;
                                break;
                            }

                            initStrategy(k);
                            double enemyscore = 0;
                            foreach (var i in mines)
                            {
                                for (int j = 0; j < N; j++)
                                {
                                    enemyscore += dist[i][j] * dist[i][j] * prob[uni.root(i)][uni.root(j)];
                                }
                            }
                            enemyscore = Math.Min(enemyscore, firstmyres * 1.5);
                            enemyscore = Math.Max(enemyscore, firstmyres * 0.5);
                            exScore -= enemyscore / (N - 1);
                        }
                    }


                    id[A, B, idP] = id[B, A, idP] = 59;
                    initStrategy();
                    foreach (var i in mines)
                    {
                        for (int j = 0; j < N; j++)
                        {
                            exScore -= dist[i][j] * dist[i][j] * prob[uni.root(i)][uni.root(j)];
                        }
                    }

                    if (best < exScore)
                    {
                        best = exScore;
                        s = A;
                        t = B;
                    }
                    id[A, B, idP] = id[B, A, idP] = 0;
                }
            }

        }

        Move2 ret = new Move2();
        if (id[s, t, 0] == 0 && id[t, s, 0] == 0)
        {
            Claim r = new Claim(p, s, t);
            ret.claim = r;
        }
        else
        {
            Option r = new Option(p, s, t);
            ret.option = r;
            options--;
        }
        moveUpdate(ret);
        //initStrategy();

        //solid score
        AllScore = 0;
        int us = uni.root(s);
        int ts = uni.root(t);
        foreach (var i in mines)
        {
            for (int j = 0; j < N; j++)
            {
                int ri = uni.root(i);
                int rj = uni.root(j);
                if (ri == rj || (ri == us && rj == ts) || (ri == ts && rj == us)) AllScore += dist[i][j] * dist[i][j];
            }
        }

        if (best < 0 || obst || true)
        {
            best = 0;
            foreach (var i in mines)
            {
                for (int j = 0; j < N; j++)
                {
                    best += dist[i][j] * dist[i][j] * prob[uni.root(i)][uni.root(j)];
                }
            }
        }
        Console.Error.WriteLine("MyID: " + punter);
        Console.Error.WriteLine("Score: " + AllScore);
        Console.Error.WriteLine("Expected Score: " + best);

        return ret;
    }

    //swap
    void swap<T>(ref T a, ref T b)
    {
        T c = a;
        a = b;
        b = c;
    }

    double getAddScore(int a, int b)
    {
        int hash = a * N + b;
        if (addScore.ContainsKey(hash)) return addScore[hash];
        double nowscore = 0;

        foreach (var c in ID2[a])
        {
            foreach (var d in ID[b])
            {
                nowscore += dist[c][d] * dist[c][d];
            }
        }
        //double ave = nowscore / ID2[a].Count / ID[b].Count;
        //nowscore += ave * (ID[a].Count + ID[b].Count);

        return addScore[hash] = nowscore;
    }


    double[] two = new double[100];

    void initStrategy(int pid = -1)
    {
        if (pid == -1) pid = punter;
        uni = new UnionFind(N);
        mark = new long[N];
        mark2 = new long[N];
        firstTime = true;
        for (int i = 0; i < N; i++)
        {
            foreach (var j in es[i])
            {
                if (id[i, j, 0] == pid + 1 || id[i, j, 1] == pid + 1)
                {
                    uni.connect(i, j);
                    firstTime = false;
                }
                else if (id[i, j, 0] != 0)
                {
                    mark[i] ^= 1L << ((id[i, j, 0] - 1) % 60);
                    mark[j] ^= 1L << ((id[i, j, 0] - 1) % 60);
                }
            }
        }

        mineEdge = 0;
        isMine = new bool[N];
        foreach (var i in mines)
        {
            //mark[i] |= 1L << 60;
            //mark[i] |= 1L << 61;
            mineEdge += es[i].Count;
            isMine[i] = true;
        }

        /*
        for (int i = 0; i < N; i++)
        {
            mark2[i] |= mark[i];
            foreach (var j in es[i])
            {
                if (id[i, j] != 0 && id[i, j] != punter + 1)
                {
                    mark2[i] |= mark[j];
                    mark2[j] |= mark[i];
                }
            }
        }
        */

        if (dist == null)
        {
            dist = new int[N][];
            foreach (var i in mines)
            {
                dist[i] = new int[N];
                for (int j = 0; j < N; j++)
                {
                    dist[i][j] = -1;
                }

                Queue<int> q = new Queue<int>();
                q.Enqueue(i);
                dist[i][i] = 0;
                while (q.Count != 0)
                {
                    int now = q.Dequeue();
                    foreach (int j in es[now])
                    {
                        if (dist[i][j] != -1) continue;
                        dist[i][j] = dist[i][now] + 1;
                        q.Enqueue(j);
                    }
                }
            }
        }

        ID = new List<int>[N];
        ID2 = new List<int>[N];
        for (int i = 0; i < N; i++)
        {
            ID[i] = new List<int>();
            ID2[i] = new List<int>();
        }

        for (int i = 0; i < N; i++)
        {
            ID[uni.root(i)].Add(i);
        }

        foreach (var i in mines)
        {
            ID2[uni.root(i)].Add(i);
        }

        double[] low = new double[N * 10];

        int allE = 0;
        int nokoriE = 0;
        for (int i = 0; i < N; i++)
        {
            foreach (var j in es[i])
            {
                if (i > j) continue;
                allE++;
                if (id[i, j, 0] == 0) nokoriE++;
            }
        }


        for (int i = 0; i < 100; i++)
        {
            two[i] = Math.Pow(0.7, i);
        }

        //double TT = Math.Sqrt(2.0 * punters * allE / nokoriE) + 1;

        double TT = 0;
        /*
        for (int i = 0; i < es.Length; i++)
        {
            foreach (var j in es[i])
            {
                if (i < j && id[i,j] == 0) TT += 1;
            }    
        }
        */
        //TT /= N;
        //TT = 3 / TT;
        //if (TT < 1) TT = 1;
        //if (TT > 4) TT = 4;
        //Console.WriteLine(TT);
        TT = 1.3 + 0.7 * (1 - (double)nokoriE / allE);
        double sum = 0;
        for (int i = 0; i < low.Length; i++)
        {
            low[i] = (1 - sum) / TT;
            sum += low[i];
        }

        prob = new double[N][];
        foreach (var ti in mines)
        {
            int i = uni.root(ti);
            if (prob[i] != null) continue;
            prob[i] = new double[N];
            prob[i][i] = 1;
            bool[] used = new bool[N];
            Heap<ProbNode> h = new Heap<ProbNode>();
            h.push(new ProbNode(i, 1));
            double[] score = new double[N];
            score[i] = 1;
            int[] num = new int[N];

            while (h.top != null)
            {
                var now = h.pop();
                if (used[now.id]) continue;
                prob[i][now.id] = score[now.id];
                used[now.id] = true;
                Dictionary<int, bool> dic = new Dictionary<int, bool>();
                foreach (var s in ID[now.id])
                {
                    //Console.WriteLine(s);
                    foreach (var to in es[s])
                    {
                        if (id[s, to, 0] != 0) continue;
                        int tid = uni.root(to);
                        if (used[tid]) continue;
                        dic[tid] = true;
                        score[tid] += low[num[tid]] * now.score;// * two[bitCount(mark[s] | mark[to]) + bitCount(mark2[s] | mark2[to])];
                        num[tid]++;
                    }
                }
                foreach (var tid in dic.Keys)
                {
                    h.push(new ProbNode(tid, score[tid]));
                }
            }


            for (int t = 0; t < 2; t++)
            {
                ProbNode[] pn = new ProbNode[N];
                for (int j = 0; j < N; j++)
                {
                    pn[j] = new ProbNode(j, prob[i][j]);
                }
                Array.Sort(pn);

                foreach (var item in pn)
                {
                    int now = item.id;
                    double nowscore = 0;
                    List<double> d = new List<double>();
                    foreach (var a in ID[now])
                    {
                        foreach (var b in es[a])
                        {
                            int bid = uni.root(b);
                            if (bid == now) continue;
                            if (id[a, b, 0] != 0) continue;
                            d.Add(prob[i][bid]);// * two[bitCount(mark[a] | mark[b]) + bitCount(mark2[a] | mark2[b])]);
                        }
                    }
                    d.Sort();
                    d.Reverse();
                    for (int tt = 0; tt < d.Count; tt++)
                    {
                        nowscore += d[tt] * low[tt];
                    }

                    prob[i][now] = Math.Max(prob[i][now], nowscore);
                }
            }
        }

        AID = new List<int>();
        BID = new List<int>();
        for (int i = 0; i < N; i++)
        {
            if (prob[i] != null) AID.Add(i);
            if (ID[i] != null) BID.Add(i);
        }
    }

    int bitCount(long x)
    {
        x = (x & 0x5555555555555555) + (x >> 1 & 0x5555555555555555);
        x = (x & 0x3333333333333333) + (x >> 2 & 0x3333333333333333);
        x = (x & 0x0f0f0f0f0f0f0f0f) + (x >> 4 & 0x0f0f0f0f0f0f0f0f);
        x = (x & 0x00ff00ff00ff00ff) + (x >> 8 & 0x00ff00ff00ff00ff);
        x = (x & 0x0000ffff0000ffff) + (x >> 16 & 0x0000ffff0000ffff);
        return (int)((x & 0x00000000ffffffff) + (x >> 32 & 0x00000000ffffffff));
    }




    void stateToMyState(string state)
    {
        string[] st = state.Split(':');
        N = int.Parse(st[0]);
        punter = int.Parse(st[1]);
        punters = int.Parse(st[2]);
        string[] st2 = st[3].Split(',');

        id = new int[N, N, 2];
        es = new List<int>[N];
        for (int i = 0; i < N; i++)
        {
            es[i] = new List<int>();
        }
        foreach (var item in st2)
        {
            string[] st3 = item.Split('-');
            int s = int.Parse(st3[0]);
            int t = int.Parse(st3[1]);
            int d = int.Parse(st3[2]);
            int d2 = int.Parse(st3[3]);
            es[s].Add(t);
            es[t].Add(s);
            id[s, t, 0] = id[t, s, 0] = d;
            id[s, t, 1] = id[t, s, 1] = d2;
        }

        st2 = st[4].Split(',');
        mines = new int[st2.Length];
        for (int i = 0; i < mines.Length; i++)
        {
            mines[i] = int.Parse(st2[i]);
        }
        AllScore = int.Parse(st[5]);
        options = int.Parse(st[6]);
    }

    public string ToState()
    {
        string ret = string.Format($"{N}:{punter}:{punters}:{esToString()}:{string.Join(",", mines)}:{AllScore}:{options}");
        return ret;
    }

    public string esToString()
    {
        List<string> ret = new List<string>();
        for (int i = 0; i < N; i++)
        {
            foreach (var j in es[i])
            {
                if (i < j) continue;
                ret.Add(string.Format($"{i}-{j}-{id[i, j, 0]}-{id[i, j, 1]}"));
            }
        }

        return string.Join(",", ret);
    }

}

class Heap<T> where T : IComparable<T>
{
    public HeapNode<T> top;

    public Heap() { }

    public void push(T val)
    {
        top = HeapNode<T>.meld(top, new HeapNode<T>(val));
    }

    public T pop()
    {
        T ret = top.val;
        top = HeapNode<T>.meld(top.r, top.l);
        return ret;
    }

    public void merge(Heap<T> h2)
    {
        top = HeapNode<T>.meld(top, h2.top);
    }

    public class HeapNode<NT> where NT : IComparable<NT>
    {
        public HeapNode<NT> l, r;
        public NT val;

        public HeapNode(NT _val)
        {
            val = _val;
        }

        public static HeapNode<NT> meld(HeapNode<NT> a, HeapNode<NT> b)
        {
            if (a == null) return b;
            if (b == null) return a;
            if (a.val.CompareTo(b.val) > 0)
            {
                HeapNode<NT> temp = a;
                a = b;
                b = temp;
            }
            a.r = meld(a.r, b);
            HeapNode<NT> temph = a.l;
            a.l = a.r;
            a.r = temph;
            return a;
        }
    }
}


class UnionFind
{

    public int[] uni;
    public UnionFind(int N)
    {
        uni = new int[N];
        for (int i = 0; i < N; i++)
        {
            uni[i] = -1;
        }
    }

    public int size(int a)
    {
        return -uni[root(a)];
    }

    public int root(int a)
    {
        if (uni[a] < 0) return a;
        else return uni[a] = root(uni[a]);
    }

    public bool connect(int a, int b)
    {
        a = root(a);
        b = root(b);
        if (a == b) return false;
        if (uni[a] > uni[b]) swap(ref a, ref b);
        uni[a] += uni[b];
        uni[b] = a;
        return true;
    }

    public void swap<T>(ref T a, ref T b)
    {
        T c = a;
        a = b;
        b = c;
    }
}


public class Myon
{
    public static void Main()
    {
        new Myon().calc();
    }

    public Myon() { }
    static DateTime StartTime;

    static public long getTime()
    {
        return (int)(DateTime.Now - StartTime).TotalMilliseconds;
    }

    public string getString(TextReader tr)
    {
        string lenS = "";
        while (true)
        {
            char c = (char)tr.Read();
            if (c == ':') break;
            lenS += c;
        }
        int Len = int.Parse(lenS);
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < Len; i++)
        {
            //Console.Error.WriteLine(sb.ToString());
            sb.Append((char)tr.Read());
        }
        return sb.ToString();
    }

    public void calc()
    {
        Console.WriteLine("18:{\"me\":\"chokudai\"}");
        string S;
        TextReader tr = Console.In;
        while (true)
        {
            S = getString(tr);
            if (S.IndexOf("stop") != -1) return;
            if (S.IndexOf("you") != -1) continue;
            break;
        }
        StartTime = DateTime.Now;
        //Console.Error.WriteLine($"JoinTime : {getTime()}");

        if (S.IndexOf("punters") != -1)
        {
            //SetUp

            MemoryStream MSinp = new MemoryStream(Encoding.UTF8.GetBytes(S));
            var Sinp = new DataContractJsonSerializer(typeof(SetUpInput));
            SetUpInput data = (SetUpInput)Sinp.ReadObject(MSinp);

            SetUpOutput ret = new SetUpOutput();

            ret.ready = data.punter;
            MyState state = new MyState(data);
            ret.state = state.ToState();

            using (var ms = new MemoryStream())
            using (var sr = new StreamReader(ms))
            {
                var serializer = new DataContractJsonSerializer(typeof(SetUpOutput));
                serializer.WriteObject(ms, ret);
                ms.Position = 0;
                var json = sr.ReadToEnd();
                Console.WriteLine($"{json.Length + 1}:{json}");
            }
        }
        //else if (S.IndexOf("stop") != -1)
        //{
        //    //stop
        //}
        else
        {
            MemoryStream MSinp = new MemoryStream(Encoding.UTF8.GetBytes(S));
            var Sinp = new DataContractJsonSerializer(typeof(MoveInput));
            //これがおそい！
            MoveInput data = (MoveInput)Sinp.ReadObject(MSinp);

            MyState state = new MyState(data);
            var tmp = state.makeClaim();

            if (tmp.claim != null)
            {
                MoveOutput ret = new MoveOutput();
                ret.claim = tmp.claim;
                ret.state = state.ToState();

                using (var ms = new MemoryStream())
                using (var sr = new StreamReader(ms))
                {
                    var serializer = new DataContractJsonSerializer(typeof(MoveOutput));
                    serializer.WriteObject(ms, ret);
                    ms.Position = 0;
                    var json = sr.ReadToEnd();
                    Console.WriteLine($"{json.Length + 1}:{json}");
                }
            }
            else
            {
                MoveOutputOption ret = new MoveOutputOption();
                ret.option = tmp.option;
                ret.state = state.ToState();

                using (var ms = new MemoryStream())
                using (var sr = new StreamReader(ms))
                {
                    var serializer = new DataContractJsonSerializer(typeof(MoveOutputOption));
                    serializer.WriteObject(ms, ret);
                    ms.Position = 0;
                    var json = sr.ReadToEnd();
                    Console.WriteLine($"{json.Length + 1}:{json}");
                }
            }
        }
        Console.Error.WriteLine($"TotalTime : {getTime()}");
    }
}