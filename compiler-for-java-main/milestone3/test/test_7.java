public class CallingMethodsInSameClass
{
	public static void main(int[] args) {
	
	}

	public static int printOne(int x) {
		System.out.println("Hello World");
		float a=2.3;
		return 1;
	}

	public static void printTwo() {
		int a = printOne(2);
	}
}
