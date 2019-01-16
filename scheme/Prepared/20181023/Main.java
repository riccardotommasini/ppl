public class Main {
	
	public static void main(String[] args){
		for (int i=0; i<5; i++){
			for (int j=0; i<5; j++){
				System.err.println("inner " + j);
				break;
			}
			System.out.println("outer " + i);
		}
	}
}