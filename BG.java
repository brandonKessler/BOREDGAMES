import java.awt.Point;
import java.util.LinkedList;
import java.util.Scanner;


public class BG {
	static Scanner input = new Scanner (System.in);
	public static LinkedList<String> Players = new LinkedList<String>();
	public static LinkedList<Pieces> PCS = new LinkedList<Pieces>();
	LinkedList<Pieces> PR = new LinkedList<Pieces>();
	public static Pieces PC;
	public static int curPlayer = 0;
	public static int rows;
	public static int cols;
	static void setup() {
		rows = 3; cols = 3;
		Players.add("A");
		Players.add("B");
		for(int IND=0; IND<6; IND++) {
		Pieces P = new Pieces("A","X",0,0,0);
		PCS.add(P);}
		for(int IND=0; IND<6; IND++) {
		Pieces P = new Pieces("B","O",0,0,0);
		PCS.add(P);}
	}
	public static int q= 1;
	public static String output;
	public static int x;
	public static int y;
	
public static void main(String[] args) {
	setup();	
	while(true) {
	for(int IND=1; IND<=3; IND++){
	if (Crd(PCS,q,1)==-1)
	{
	output = " _ ";
	}
	else
	{
	output = PCS.get( Crd_Pos(PCS,q,1,1) ).name;
	}
	if (Crd(PCS,q,2)==-1)
	{
	output = output+" _ ";
	}
	else
	{
	output = output+PCS.get( Crd_Pos(PCS,q,2,1) ).name;
	}
	if (Crd(PCS,q,3)==-1)
	{
	output = output+" _ ";
	}
	else
	{
	output = output+PCS.get( Crd_Pos(PCS,q,3,1) ).name;
	}
	System.out.println(output);
	q++;
	}
	x = Integer.parseInt(input.nextLine());;
	y = Integer.parseInt(input.nextLine());;
	if (R3())
	{
	PC = PCS.get( Crd_Plr_Pos(PCS,0,0,Players.get(curPlayer),1) );
	PC.loc.x = x;
	PC.loc.y = y;
	if (R9())
	{
	System.out.println("Player "+Players.get(curPlayer)+" Won"); System.exit(0);
	}
	else
	{
	if (R6())
	{
	System.out.println("Board Full! No Winner!"); System.exit(0);
	}
	}
	NP();}

	}




}

/*static void printBoard() {
	System.out.println("Inventory:");
	LinkedList<Pieces> pr = PrintCrd(PCS,0,0);
	for (int IND = 0; IND<pr.size(); IND++) {
		Pieces p = pr.get(IND);
		System.out.println(p.owner+ "\t"+p.name+"\t"+p.val);
	}
	pr.clear();
	for (int XP=1; XP<cols; XP++) {
		for (int YP=1; YP<cols; YP++) {
			pr = PrintCrd(PCS,0,0);
			System.out.println("\nLocation: ("+XP+", "+YP+")");
			for (int IND = 0; IND<pr.size(); IND++) {
				Pieces p = pr.get(IND);
				System.out.println(p.owner+ "\t"+p.name+"\t"+p.val);
			}
			pr.clear();
		}
	}
}

static Pieces[] PrintCrd (LinkedList<Pieces> plist, int x, int y) {
	LinkedList<Pieces> ret = new LinkedList<Pieces>();
	Point test = new Point(x,y);
	for (int i=0; i<plist.size(); i++){
		Pieces t = plist.get(i);
		if (t.loc.equals(test)) {
			ret.add(t);
		}
	}
	return ret;
}*/

static void NP () {
	curPlayer++;
	if (curPlayer > Players.size()-1) {
		curPlayer=0;
	}
}

static int search (LinkedList<Pieces> plist, Pieces p) {
	for (int i=0; i<plist.size(); i++){
		Pieces t = plist.get(i);
		if (t.equals(p)) {
			return i;
		}
	}
	return -1;
}

static int Crd_Plr_SearchCt (LinkedList<Pieces> plist, int x, int y, String plr) {
	int count = 0;
	Point test = new Point(x,y);
	for (int i=0; i<plist.size(); i++){
		Pieces t = plist.get(i);
		if (t.loc.equals(test)) {
			count++;
		}
	}
	return count;
}
static int Crd_Plr_SearchCt_Gt (LinkedList<Pieces> plist, int x, int y, String plr) {
	int count = 0;
	Point test = new Point(x,y);
	for (int i=0; i<plist.size(); i++){
		Pieces t = plist.get(i);
		if (t.loc.x > test.x && t.loc.y>test.y) {
			count++;
		}
	}
	return count;
}


static int Pc_Pos (LinkedList<Pieces> plist, Pieces p, int pos) {
	int count = pos;
	for (int i=0; i<plist.size(); i++){
		Pieces t = plist.get(i);
		if (t.equals(p)) {
			if (count==1) {
				return i;
			}
			count--;
		}
		
	}
	return -1;
}

static int Crd_Plr_Pos (LinkedList<Pieces> plist, int x, int y, String plr, int pos) {
	int count = pos;
	Point test = new Point(x,y);
	for (int i=0; i<plist.size(); i++){
		Pieces t = plist.get(i);
		if (t.loc.equals(test) && t.owner.equals(plr)) {
			if (count==1) {
				return i;
			}
			count--;
		}
	}
	return -1;
}
static int Crd_Plr_Pos_Gt (LinkedList<Pieces> plist, int x, int y, String plr, int pos) {
	int count = pos;
	Point test = new Point(x,y);
	for (int i=0; i<plist.size(); i++){
		Pieces t = plist.get(i);
		if (t.loc.x > test.x && t.loc.y > test.y && t.owner.equals(plr)) {
			if (count==1) {
				return i;
			}
			count--;
		}
	}
	return -1;
}

static int Crd_Pos (LinkedList<Pieces> plist, int x, int y, int pos) {
	int count = pos;
	Point test = new Point(x,y);
	for (int i=0; i<plist.size(); i++){
		Pieces t = plist.get(i);
		if (t.loc.equals(test)) {
			if (count==1) {
				return i;
			}
			count--;
		}
	}
	return -1;
}


static int Crd_Plr (LinkedList<Pieces> plist, int x, int y, String plr) {
	Point test = new Point(x,y);
	for (int i=0; i<plist.size(); i++){
		Pieces t = plist.get(i);
		if (t.loc.equals(test) && t.owner.equals(plr)) {
			return i;
		}
	}
	return -1;
}

static int Crd_Plr_Pcn (LinkedList<Pieces> plist, int x, int y, String plr, String pcn) {
	Point test = new Point(x,y);
	for (int i=0; i<plist.size(); i++){
		Pieces t = plist.get(i);
		if (t.loc.equals(test) && t.owner.equals(plr) && t.name.equals(pcn)) {
			return i;
		}
	}
	return -1;
}
static int Pcn (LinkedList<Pieces> plist, String pcn) {
	for (int i=0; i<plist.size(); i++){
		Pieces t = plist.get(i);
		if (t.name.equals(pcn)) {
			return i;
		}
	}
	return -1;
}
static int Plr_Pcn (LinkedList<Pieces> plist, String plr, String pcn) {
	for (int i=0; i<plist.size(); i++){
		Pieces t = plist.get(i);
		if (t.name.equals(pcn) && t.owner.equals(plr)) {
			return i;
		}
	}
	return -1;
}
static int Crd_Pcn (LinkedList<Pieces> plist, int x, int y, String pcn) {
	Point test = new Point(x,y);
	for (int i=0; i<plist.size(); i++){
		Pieces t = plist.get(i);
		if (t.loc.equals(test) && t.name.equals(pcn)) {
			return i;
		}
	}
	return -1;
}
static int Crd (LinkedList<Pieces> plist, int x, int y) {
	Point test = new Point(x,y);
	for (int i=0; i<plist.size(); i++){
		Pieces t = plist.get(i);
		if (t.loc.equals(test)) {
			return i;
		}
	}
	return -1;
}

static int Crd_Plr_Gt (LinkedList<Pieces> plist, int x, int y,String plr) {
	Point test = new Point(x,y);
	for (int i=0; i<plist.size(); i++){
		Pieces t = plist.get(i);
		if (t.loc.x>test.x && t.loc.y>test.y && t.owner.equals(plr)) {
			return i;
		}

	}
	return -1;
}
static boolean R1() {
if (x< 3&& x> 0)
{
return true;
}
else
{
return false;
}
}
static boolean R2() {
if (y< 3&& y> 0)
{
return true;
}
else
{
return false;
}
}
static boolean R3() {
if (R1()&& R2())
{
return true;
}
else
{
return false;
}
}
static boolean R4() {
return Crd(PCS,x,y)==-1;
}
static boolean R5() {
int i= 1;
int j= 1;
boolean full= true;
while (i< 4) {
while (j< 4) {
if (Crd(PCS,i,j)==-1)
{
full = false;
}
j = j+ 1;
}
i = i+ 1;
}

return full;
}
static boolean R6() {
int k= 1;
int h= 1;
int val= 0;
boolean win= false;
while (k< 4) {
while (h< 4) {
if (Crd(PCS,h,k)==-1)
{
win = false;
}
else
{
if (PCS.get( Crd_Pos(PCS,h,k,1) ).owner.equals(Players.get(curPlayer)))
{
val = val+ 1;
}
else
{
val = val;
}
}
h = h+ 1;
}
if (val== 3)
{
win = true;
}
else
{
val = 0;
k = k+ 1;
}
}
System.out.println("R6:"+win);
return win;
}
static boolean R7() {
int l= 1;
int m= 1;
boolean winChance;
while (l< 4) {
winChance = true;
m=1;
while (m< 4) {
if ((Crd(PCS,m,l)==-1)== false)
{
	System.out.println(PCS.get( Crd_Pos(PCS,m,l,1) ).name);
	System.out.println(PCS.get( Crd_Plr_Pos(PCS,0,0,Players.get(curPlayer),1) ).name);
	System.out.println(PCS.get( Crd_Pos(PCS,m,l,1) ).name.equals(PCS.get( Crd_Plr_Pos(PCS,0,0,Players.get(curPlayer),1) ).name));
if (winChance== true&& false== PCS.get( Crd_Pos(PCS,m,l,1) ).name.equals(PCS.get( Crd_Plr_Pos(PCS,0,0,Players.get(curPlayer),1) ).name))
{
winChance = false;
}
}
else
{
winChance = false;
}
m = m+ 1;
}
if (winChance== true)
{
	System.out.println("R7:true");

return true;
}
l = l+ 1;
}
System.out.println("R7:false");

return false;
}


static boolean R8() {
int n= 1;
int a= 0;
for(int IND=1; IND<=3; IND++){
if (Crd(PCS,n,n)==-1== false&& PCS.get( Crd_Pos(PCS,n,n,1) ).name.equals(PCS.get( Crd_Plr_Pos(PCS,0,0,Players.get(curPlayer),1) ).name))
{
a = a+ 1;
}
n = n+ 1;
}

if (a== 3)
{
	System.out.println("R8:true");

return true;
}
else
{
	System.out.println("R8:false");

return false;
}
}
static boolean R9() {
if (R6()|| R7()|| R8())
{
return true;
}
else
{
return false;
}
}




}

