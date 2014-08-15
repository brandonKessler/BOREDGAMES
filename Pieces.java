import java.awt.Point;


public class Pieces {

	public String owner;
	public String name;
	public int val;
	public Point loc;
	
	Pieces(String own, String nm, int v, int x, int y) {
		owner = new String(own);
		name = new String(nm);
		val = v;
		loc = new Point(x,y);
	}
	
	public boolean equals(Pieces p) {
		boolean o = this.owner.equals(p.owner);
		boolean n = this.name.equals(p.name);
		boolean v =  this.val == p.val;
		boolean l =  this.loc.equals(p.loc);
		return (o&&n&&v&&l);
	}
	
	public int hashCode() {
		return 0;
	}
}

