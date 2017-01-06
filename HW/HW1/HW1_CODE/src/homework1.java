import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.PrintWriter;
import java.io.UnsupportedEncodingException;
import java.net.URL;
import java.util.*;

class City{

	String cityName;

	int pathCost;

	String parent = null;

	int nodeNumber = -1;

	//only for A* search
	int heuristic = 0;

	int priorityQueueNumber = 0;

	public City(String city, int distance, int nodeNumber) {
		this.cityName = city;
		this.pathCost = distance;
		this.nodeNumber = nodeNumber;
	}
}


class PriorityQueueComparator implements Comparator<City> {
	public int compare(City A, City B) {
		if(B.pathCost!=A.pathCost){
			return  A.pathCost - B.pathCost;
		}else{
			if(A.parent.equals(B.parent)){
				return  A.nodeNumber - B.nodeNumber;	
			}else{
				return  A.priorityQueueNumber - B.priorityQueueNumber;
			}
		}
	}
}

class PriorityQueueComparatorAStar implements Comparator<City> {
	public int compare(City A, City B) {
		if((B.pathCost+B.heuristic)!=(A.pathCost+A.heuristic)){
			return A.pathCost+A.heuristic-B.pathCost-B.heuristic;
		}else{
			if(A.parent.equals(B.parent)){
				return  A.nodeNumber - B.nodeNumber;	
			}else{
				return  A.priorityQueueNumber - B.priorityQueueNumber;
			}
		}
	}
}


public class homework1
{
	public static void main(String args[])
	{    
		Scanner scanner = null;

		String initialState = null;

		String goalState = null;

		String algorithm = null;

		int noOfPaths = 0;

		URL url = homework1.class.getResource("input.txt");

		File file = new File(url.getPath());

		try{
			scanner = new Scanner(new FileReader(file));
		}catch(Exception e){
			e.printStackTrace();
		}

		if(scanner!=null){
			if(scanner.hasNext()){
				//Get the source
				algorithm = scanner.next();
			}else{
				System.out.println("Issue in reading algorithm");
			}	

			if(scanner.hasNext()){
				//Get the source
				initialState = scanner.next();
			}else{
				System.out.println("Issue in reading initial state");
			}

			if(scanner.hasNext()){
				//Get the source
				goalState = scanner.next();
			}else{
				System.out.println("Issue in reading goal state");
			}
		}

		if (scanner.hasNextInt())
		{
			//get the number of paths
			noOfPaths = scanner.nextInt();
		}	
		else{
			System.out.println("Issue in reading number of paths");
		}	


		Map<String,List<City>> adjacencyList= new HashMap<String, List<City>>();		
		for(int i=0 ; i < noOfPaths ; i++){
			/*If suppose A-->B are the cities
			Add (A,List<AdjacentCity>) into hashmap if it doesn't exist.
			If B doesn't exist add B into the list
			Add (A,List<AdjacentCity>) into hashmap if it doesn't exist.
			 */
			String startCity = scanner.next();
			String destinationCity = scanner.next();
			int distance = scanner.nextInt();

			if(adjacencyList.containsKey(startCity)){
				adjacencyList.get(startCity).add(new City(destinationCity,distance,i+1));
			}else{
				List<City> cityList = new ArrayList<City>();
				cityList.add(new City(destinationCity,distance,i+1));
				adjacencyList.put(startCity, cityList);

			}

			if(!adjacencyList.containsKey(destinationCity)){
				List<City> cityList = new ArrayList<City>();
				adjacencyList.put(destinationCity, cityList);
			}
		}	

		List<City> mResult = null; 

		if(algorithm.equals("BFS")){
			mResult = performBFS(initialState,adjacencyList,goalState);
		}
		else if(algorithm.equals("DFS")){
			mResult = performDFS(initialState,adjacencyList,goalState);
		}
		else if(algorithm.equals("UCS")){
			mResult = performUCS(initialState,adjacencyList,goalState);
		}
		else if(algorithm.equals("A*")){
			int mCount =0;
			HashMap<String, Integer> mHeuristicMap = new HashMap<String, Integer>();
			if(scanner.hasNext()){
				mCount = scanner.nextInt();
			}else{
				System.out.println("Issue in mCount Reading");
			}
			for(int i =0; i< mCount ;i++){
				String key = "";
				int value = 0;
				if(scanner.hasNext()){
					key = scanner.next();
				}else{
					System.out.println("Issue in key Reading");
				}
				if(scanner.hasNext()){
					value = scanner.nextInt();
				}else{
					System.out.println("Issue in value Reading");
				}
				mHeuristicMap.put(key, value);
			}

			mResult = performAStar(initialState,adjacencyList,goalState,mHeuristicMap);
		}

		if(mResult == null){
			System.out.println("Path not found");	
		}else{
			PrintWriter writer=null;
			try {
				writer = new PrintWriter("output.txt", "UTF-8");
			} catch (FileNotFoundException | UnsupportedEncodingException e1) {
				e1.printStackTrace();
			}
			for(int i = mResult.size()-1; i>-1; i--){
				if(writer!=null)
					writer.println(mResult.get(i).cityName + " " + mResult.get(i).pathCost);
				System.out.println(mResult.get(i).cityName + " " + mResult.get(i).pathCost);
			}
			try{
				writer.close();
			}catch (Exception e) {
				e.printStackTrace();
			}

		}
	}

	private static List<City> performAStar(String initialState, Map<String, List<City>> adjacencyList, String goalState,
			HashMap<String, Integer> mHeuristicMap) {

		//Result
		List<City> mResult =  new ArrayList<City>();
		//InputAdjacencyList
		HashMap<String, List<City>> mInputAdjacencyList = (HashMap<String, List<City>>)adjacencyList;
		//VistedMap
		HashMap<String,City> mVisitedMap= new HashMap<String,City>();
		//CurrentQueue
		PriorityQueue<City> mPriorityQueue = new PriorityQueue<City>(1, new PriorityQueueComparatorAStar());
		//In Priority Queue
		HashMap<String , Integer> mInPriorityQueue = new HashMap<String, Integer>();
		
		int priorityQueueNumber = 0;

		City mInitialCity = new City(initialState,0,0);
		mInitialCity.priorityQueueNumber = priorityQueueNumber;
		priorityQueueNumber++;
		mInitialCity.heuristic = mHeuristicMap.get(initialState);
		mPriorityQueue.add(mInitialCity);
		mInPriorityQueue.put(initialState,mHeuristicMap.get(initialState)); // 0 + mHeuristicMap.get(initialState)

		while(!mPriorityQueue.isEmpty()){
			City mCurrentState = mPriorityQueue.poll();
			System.out.println("polling order" + " " + mCurrentState.cityName);
			mInPriorityQueue.remove(mCurrentState.cityName);
			if(mCurrentState.cityName.equals(goalState)){
				City mResultCity = mCurrentState;
				do{
					mResult.add(mResultCity);
					mResultCity = mVisitedMap.get(mResultCity.parent);
				}while(mResultCity.parent!=null);
				mResult.add(mResultCity);
				return mResult;
			}
			mVisitedMap.put(mCurrentState.cityName, mCurrentState);
			ArrayList<City> mNeighbours = (ArrayList<City>)mInputAdjacencyList.get(mCurrentState.cityName);
			int cost = mCurrentState.pathCost;

			for(int i=0; i<mNeighbours.size();i++){
				City mNeighbour = mNeighbours.get(i);
				City mNewNeighbour = new City(mNeighbour.cityName, cost + mNeighbour.pathCost, mNeighbour.nodeNumber);
				mNewNeighbour.parent = mCurrentState.cityName;
				//mNeighbour.pathCost =  + cost;
				mNewNeighbour.heuristic = mHeuristicMap.get(mNeighbour.cityName);
				if(mInPriorityQueue.containsKey(mNewNeighbour.cityName)){
					if(mInPriorityQueue.get(mNewNeighbour.cityName)>(mNewNeighbour.pathCost+mNewNeighbour.heuristic)){
						City mTempCity = null;
						for (City mCurrentCity : mPriorityQueue) {
							if(mCurrentCity.cityName.equals(mNewNeighbour.cityName)){
								//change the cost and the parent of the Node in the priority Queue
								mTempCity = mCurrentCity;

							}
						}

						mPriorityQueue.remove(mTempCity);
						//change the cost in the mInPriorityQueue
						mNewNeighbour.priorityQueueNumber = priorityQueueNumber;
						priorityQueueNumber++;
						mPriorityQueue.add(mNewNeighbour);
						mInPriorityQueue.put(mTempCity.cityName, mNewNeighbour.pathCost + mNewNeighbour.heuristic);
					}
				}else{
					//Add the new City into priority queue
					if(!mVisitedMap.containsKey(mNewNeighbour.cityName)){
						mNewNeighbour.priorityQueueNumber = priorityQueueNumber;
						priorityQueueNumber++;
						mPriorityQueue.add(mNewNeighbour);
						//Add the new node into the mInPriorityQueue map
						mInPriorityQueue.put(mNewNeighbour.cityName, mNewNeighbour.pathCost + mNewNeighbour.heuristic);
					}else{
						City mCity = mVisitedMap.get(mNewNeighbour.cityName);
						if((mCity.pathCost + mCity.heuristic) > (mNewNeighbour.pathCost + mNewNeighbour.heuristic)){
							//Remove from the visited map and put in the priority queue
							mVisitedMap.remove(mNewNeighbour.cityName);
							mNewNeighbour.priorityQueueNumber = priorityQueueNumber;
							priorityQueueNumber++;
							mPriorityQueue.add(mNewNeighbour);
							//change the cost in the mInPriorityQueue
							mInPriorityQueue.put(mCity.cityName, mNewNeighbour.pathCost + mNewNeighbour.heuristic);
						}
					}
				}
			}

		}

		return null;
	}

	private static List<City> performUCS(String initialState, Map<String, List<City>> adjacencyList, String goalState) {

		//Result
		List<City> mResult =  new ArrayList<City>();
		//InputAdjacencyList
		HashMap<String, List<City>> mInputAdjacencyList = (HashMap<String, List<City>>)adjacencyList;
		//VistedMap
		HashMap<String,City> mVisitedMap= new HashMap<String,City>();
		//CurrentQueue
		PriorityQueue<City> mPriorityQueue = new PriorityQueue<City>(1, new PriorityQueueComparator());
		//In Priority Queue
		HashMap<String , Integer> mInPriorityQueue = new HashMap<String, Integer>();

		int priorityQueueNumber = 0;

		City initialCity = new City(initialState,0,0);
		initialCity.priorityQueueNumber = priorityQueueNumber;
		priorityQueueNumber++;
		mPriorityQueue.add(initialCity);
		mInPriorityQueue.put(initialState, 0);

		while(!mPriorityQueue.isEmpty()){
			City mCurrentState = mPriorityQueue.poll();
			mInPriorityQueue.remove(mCurrentState.cityName);
			if(mCurrentState.cityName.equals(goalState)){
				City mResultCity = mCurrentState;
				do{
					mResult.add(mResultCity);
					mResultCity = mVisitedMap.get(mResultCity.parent);
				}while(mResultCity.parent!=null);
				mResult.add(mResultCity);
				return mResult;
			}
			mVisitedMap.put(mCurrentState.cityName, mCurrentState);
			ArrayList<City> mNeighbours = (ArrayList<City>)mInputAdjacencyList.get(mCurrentState.cityName);
			int cost = mCurrentState.pathCost;

			for(int i=0; i<mNeighbours.size();i++){
				City mNeighbour = mNeighbours.get(i);
				City mNewNeighbour = new City(mNeighbour.cityName, cost + mNeighbour.pathCost, mNeighbour.nodeNumber);
				mNewNeighbour.parent = mCurrentState.cityName;

				//mNewNeighbour.pathCost = mNewNeighbour.pathCost + cost;
				if(mInPriorityQueue.containsKey(mNewNeighbour.cityName)){
					if(mInPriorityQueue.get(mNewNeighbour.cityName)>mNewNeighbour.pathCost){
						City temp = null;
						for (City mCurrentCity : mPriorityQueue) {
							if(mCurrentCity.cityName.equals(mNewNeighbour.cityName)){
								temp = mCurrentCity;
							}
						}
						//change the cost and the parent of the Node in the priority Queue
						mPriorityQueue.remove(temp);
						mNewNeighbour.priorityQueueNumber = priorityQueueNumber;
						priorityQueueNumber++;
						mPriorityQueue.add(mNewNeighbour);
						//change the cost in the mInPriorityQueue
						mInPriorityQueue.put(mNewNeighbour.cityName, mNewNeighbour.pathCost);
					}
				}else{
					//Add the new City into priority queue
					if(!mVisitedMap.containsKey(mNewNeighbour.cityName)){
						mNewNeighbour.priorityQueueNumber = priorityQueueNumber;
						priorityQueueNumber++;
						mPriorityQueue.add(mNewNeighbour);
						//Add the new node into the mInPriorityQueue map
						mInPriorityQueue.put(mNewNeighbour.cityName, mNewNeighbour.pathCost);
					}else{
						City mCity = mVisitedMap.get(mNewNeighbour.cityName);
						if(mCity.pathCost > mNewNeighbour.pathCost){
							//Remove from the visited map and put in the priority queue
							mVisitedMap.remove(mNewNeighbour.cityName);
							mNewNeighbour.priorityQueueNumber = priorityQueueNumber;
							priorityQueueNumber++;
							mPriorityQueue.add(mNewNeighbour);
							//change the cost in the mInPriorityQueue
							mInPriorityQueue.put(mCity.cityName, mNewNeighbour.pathCost);

						}
					}
				}
			}

		}

		return null;
	}

	private static List<City> performDFS(String initialState, Map<String, List<City>> adjacencyList, String goalState) {		
		//Result
		List<City> mResult =  new ArrayList<City>();
		//InputAdjacencyList
		HashMap<String, List<City>> mInputAdjacencyList = (HashMap<String, List<City>>)adjacencyList;
		//VistedMap
		HashMap<String,City> mVisitedMap= new HashMap<String,City>();

		HashMap<String, Integer> mInStackMap = new HashMap<String,Integer>();
		//CurrentQueue
		Stack<City> mCurrentStack = new Stack<City>();	
		//check if the initial state is the goal state
		if(initialState.equals(goalState)){
			City mCity = new City(initialState,0,0);
			mResult.add(mCity);
			return mResult;
		}

		mCurrentStack.push(new City(initialState,0,0));
		mInStackMap.put(initialState, 0);

		while(!mCurrentStack.isEmpty()){
			//read all values of the stack

			City mCurrentState = mCurrentStack.pop();
			mInStackMap.remove(mCurrentState.cityName);

			if(mCurrentState.cityName.equals(goalState)){
				City mResultCity = mCurrentState;
				do{
					mResult.add(mResultCity);
					mResultCity = mVisitedMap.get(mResultCity.parent);
				}while(mResultCity.parent!=null);
				mResult.add(mResultCity);
				return mResult;
			}
			mVisitedMap.put(mCurrentState.cityName,mCurrentState);
			//put all its children in the queue and before putting just check
			//a. If it forms a loop
			//b. If it is the goalState
			ArrayList<City> mListOfNeighbours = (ArrayList<City>) mInputAdjacencyList.get(mCurrentState.cityName);
			int depth = mCurrentState.pathCost;

			for(int i = mListOfNeighbours.size()-1; i>-1; i--){
				City mNeighbour = mListOfNeighbours.get(i);
				City mNewNeighbour = new City(mNeighbour.cityName, depth+1, mNeighbour.nodeNumber);
				mNewNeighbour.parent = mCurrentState.cityName;

				if(!mVisitedMap.containsKey(mNewNeighbour.cityName)){
					if(mInStackMap.containsKey(mNeighbour.cityName)){
						if(mInStackMap.get(mNeighbour.cityName)>mNeighbour.pathCost){
							//remove from the stack, it might be in somewhere between
							Stack<City> mTempStack = new Stack<City>();
							City TempCity = mCurrentStack.pop();
							while(TempCity!=null && TempCity.cityName!=mNeighbour.cityName){
								mTempStack.push(TempCity);
								TempCity = mCurrentStack.pop();
							}
							TempCity = mTempStack.pop();
							while(TempCity!=null){
								mCurrentStack.push(TempCity);
								TempCity = mTempStack.pop();
							}
							mCurrentStack.push(mNewNeighbour);
							mInStackMap.put(mNeighbour.cityName,depth+1);
							//put the new one on the top the stack
						}else{
							//does priority come in
						}
					}else{
						mCurrentStack.push(mNewNeighbour);
						mInStackMap.put(mNeighbour.cityName,depth+1);
					}
				}
			}
		}
		return null;}

	private static List<City> performBFS(String initialState, Map<String, List<City>> adjacencyList, String goalState) {
		//Result
		List<City> mResult =  new ArrayList<City>();
		//InputAdjacencyList
		HashMap<String, List<City>> mInputAdjacencyList = (HashMap<String, List<City>>)adjacencyList;
		//VistedMap
		HashMap<String,City> mVisitedMap= new HashMap<String,City>();
		//CurrentQueue
		Queue<City> mCurrentQueue = new LinkedList<City>();	
		//check if the initial state is the goal state
		if(initialState.equals(goalState)){
			City mCity = new City(initialState, 0,0);
			mResult.add(mCity);
			return mResult;
		}
		mCurrentQueue.add(new City(initialState,0,0));

		while(!mCurrentQueue.isEmpty()){
			City mCurrentState = mCurrentQueue.poll();
			mVisitedMap.put(mCurrentState.cityName,mCurrentState);
			//put all its children in the queue and before putting just check
			//a. If it forms a loop
			//b. If it is the goalState
			ArrayList<City> mListOfNeighbours = (ArrayList<City>) mInputAdjacencyList.get(mCurrentState.cityName);
			int depth = mCurrentState.pathCost;

			for(City mNeighbour : mListOfNeighbours){
				City mNewNeighbour = new City(mNeighbour.cityName, depth+1, mNeighbour.nodeNumber);
				mNewNeighbour.parent = mCurrentState.cityName;
				/*mNeighbour.parent = mCurrentState.cityName;
				mNeighbour.pathCost = depth+1;*/
				if(mNewNeighbour.cityName.equals(goalState)){
					City mResultCity = mNewNeighbour;
					do{
						mResult.add(mResultCity);
						mResultCity = mVisitedMap.get(mResultCity.parent);
					}while(mResultCity.parent!=null);
					mResult.add(mResultCity);
					return mResult;
				}
				else if(!mVisitedMap.containsKey(mNewNeighbour.cityName)){
					mCurrentQueue.add(mNewNeighbour);
				}
			}
		}
		return null;
	}
}