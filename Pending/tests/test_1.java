public class PrimitiveParameters
{	
	public static void main(String[] args)
	{	go();
	}
	
	public static void go()
	{	int x = 3;
		int y = 2;
		System.out.println(1);
		falseSwap(x,y);
		System.out.println(2);
		moreParameters(x,y);
		System.out.println(3);
	}
	
	public static void falseSwap(int x, int y)
	{	System.out.println(4);
		int temp = x;
		x = y;
		y = temp;
		System.out.println(5);
	}
	
	public static void moreParameters(int a, int b)
	{	System.out.println(6);
		a = a * b;
		b = 12;
		System.out.println(6);
		falseSwap(b,a);
		System.out.println(7);	
	}
}