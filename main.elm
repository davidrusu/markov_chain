import Markdown
import Html exposing (br, div, button, text, input, textarea, Attribute, p, span)
import Html.Attributes exposing (value, tabindex, style)
import Html.Events exposing (..)
import Dict
import List
import String
import Random
import Json.Decode exposing (customDecoder)

import Css exposing (Styles, setViewport)
import Css.Flex as Flex
import Css.Display as Display exposing (display)
import Css.Shadow as Shadow
import Css.Background as Background
import Css.Text as Text
import Css.Font as Font
import Css.Padding as Padding
import Css.Dimension as Dim
import Css.Margin as Margin

centered : Styles -> Styles
centered styles =
  styles
    |> display Display.Flex
    |> Flex.justifyContent Flex.JCCenter
    |> Flex.alignItems Flex.AICenter

type alias State = String
                 
initState : State
initState = ""
            
errorState : State
errorState = "ERROR!!!"

type alias MarkovChain = Dict.Dict State (List (Float, State))
type alias Model = { data : String
                   , markovChain : MarkovChain
                   , memory : Int
                   , seed : Random.Seed
                   , trainingData : String
                   , errorMsg : Maybe String
                   }

initTrainingData : String
initTrainingData = "CHAPTER I In the late summer of that year we lived in a house in a village that looked across the river and the plain to the mountains. In the bed of the river there were peb- bles and boulders, dry and white in the sun, and the water was clear and swiftly moving and blue in the channels. Troops went by the house and down the road and the dust they raised powdered the leaves of the trees. The trunks of the trees too were dusty and the leaves fell early that year and we saw the troops march- ing along the road and the dust rising and leaves, stirred by the breeze, falling and the soldiers marching and afterward the road bare and white except for the leaves. The plain was rich with crops; there were many orchards of fruit trees and beyond the plain the moun- tains were brown and bare. There was fighting in the mountains and at night we could see the flashes from the artillery. In the dark it was like summer lightning, but the nights were cool and there was not the feeling of a storm coming. Sometimes in the dark we heard the troops marching under the window and guns going past pulled by motor- tractors. There was much traffic at night and many mules on the roads with boxes of ammunition on each side of their pack-saddles and gray motor-trucks that carried men, and other trucks with loads covered with canvas that moved slower in the traffic. There were big guns too that passed in the day drawn by tractors, the long barrels of the guns covered with green branches and green leafy branches and vines laid over the trac- tors. To the north we could look across a valley and see 3 4 A FAREWELL TO ARMS a forest of chestnut trees and behind it another moun- tain on this side of the river. There was fighting for that mountain too, but it was not successful, and in the fall when the rains came the leaves all fell from the chestnut trees and the branches were bare and the trunks black with rain. The vineyards were thin and bare- branched too and all the country wet and brown and dead with the autumn. There were mists over the river and clouds on the mountain and the trucks splashed mud on the road and the troops were muddy and wet in their capes; their rifles were wet and under their capes the two leather cartridge-boxes on the front of the belts, gray leather boxes heavy with the packs of clips of thin, long 6.5 mm. cartridges, bulged forward under the capes so that the men, passing on the road, marched as though they were six months gone with child. There were small gray motor-cars that passed going very fast ; usually there was an officer on the seat with the driver and more officers in the back seat. They splashed more mud than the camions even and if one of the officers in the back was very small and sitting be- tween two generals, he himself so small that you could not see his face but only the top of his cap and his nar- row back, and if the car went especially fast it was probably the King. He lived in Udine and came out in this way nearly every day to see how things were going, and things went very badly. At the start of the winter came the permanent rain and with the rain came the cholera. But it was checked and in the end only seven thousand died of it in the army, CHAPTER II The next year there were many victories. The mountain that was beyond the valley and the hillside where the chestnut forest grew was captured and there were victories beyond the plain on the plateau to the south and we crossed the river in August and lived in a house in Gorizia that had a fountain and many thick shady trees in a walled garden and a wistaria vine purple on the side of the house. Now the fighting was in the next mountains beyond and was not a mile away. The town was very nice and our house was very fine. The river ran behind us and the town had been cap- tured very handsomely but the mountains beyond it could not be taken and I was very glad the Austrians seemed to want to come back to the town some time, if the war should end, because they did not bombard it to destroy it but only a little in a military way. People lived on in it and there were hospitals and cafes and artillery up side streets and two bawdy houses, one for troops and one for officers, and with the end of the summer, the cool nights, the fighting in the mountains beyond the town, the shell-marked iron of the railway bridge, the smashed tunnel by the river where the fight- ing had been, the trees around the square and the long avenue of trees that led to the square; these with there being girls in the town, the King passing in his motor car, sometimes now seeing his face and little long necked body and gray beard like a goat's chin tuft ; all these with the sudden interiors of houses that had lost a wall through shelling, with plaster and rubble in their gardens and sometimes in the street, and the whole thing s 6 A FAREWELL TO ARMS I i going well on the Carso made the fall very different from the last fall when we had been in the country. The war was changed too. The forest of oak trees on the mountain beyond the town was gone. The forest had been green in the summer when we had come into the town but now there were the stumps and the broken trunks and the ground torn up, and one day at the end of the fall when I was out where the oak forest had been I saw a cloud com- ing over the mountain. It came very fast and the sun went a dull yellow and then everything was gray and the sky was covered and the cloud came on down the mountain and suddenly we were in it and it was snow. The snow slanted across the wind, the bare ground was covered, the stumps of trees projected, there was snow on the guns and there were paths in the snow going back to the latrines behind trenches. Later, below in the town, I watched the snow falling, looking out of the window of the bawdy house, the house for officers, where I sat with a friend and two glasses drinking a bottle of Asti, and, looking out at the snow falling slowly and heavily, we knew it was all over for that year. Up the river the mountains had not been taken ; none of the mountains beyond the river had been taken. That was all left for next year. My friend saw the priest from our mess going by in the street, walking carefully in the slush, and pounded on the win- dow to attract his attention. The priest looked up. He saw us and smiled. My friend motioned for him to come in. The priest shook his head and went on. That night in the mess after the spaghetti course, which every one ate very quickly and seriously, lifting the spaghetti on the fork until the loose strands hung clear then lowering it into the mouth, or else using a con- A FAREWELL TO ARMS 7 tinuous lift and sucking into the mouth, helping our- selves to wine from the grass-covered gallon flask; it swung in a metal cradle and you pulled the neck of the flask down with the forefinger and the wine, c]g ar red, tannic and lovely, poured out into the glass held with the same hand; after this course, the captain com- menced picking on the priest. The priest was young and blushed easily and wore a uniform like the rest of us but with a cross in dark red velvet above the left breast pocket of his gray tunic. The captain spoke pidgin Italian for my doubt- ful benefit, in order that I might understand perfectly, that nothing should be lost. Priest to-day with girls, the captain said looking at the priest and at me. The priest smiled and blushed and shook his head. This captain baited him often. Not true? asked the captain. To-day I see priest with girls. No, said the priest. The other officers were amused at the baiting. Priest not with girls, went on the captain. Priest never with girls, he explained to me. He took my glass and filled it, looking at my eyes all the time, but not losing sight of the priest. Priest every night five against one. Every one at the table laughed. You understand ? Priest every night five against one. He made a gesture and laughed loudly. The priest accepted it as a joke. The Pope wants the Austrians to win the war, the major said. He loves Franz Joseph. That's where the money comes from. I am an atheist. Did you ever read the Black Pig ? asked the lieu- tenant. I will get you a copy. It was that which shook my faith. 8 A FAREWELL TO ARMS It is a filthy and vile book/' said the priest. You do not really like it It is very valuable/' said the lieutenant. It tells you about those priests. You will like it, he said to me. I smiled at the priest and he smiled back across the candle- light. Don't you read it, he said. I will get it for you, said the lieutenant. All thinking men are atheists/' the major said. I do not believe in the Free Masons however. I believe in the Free Masons, the lieutenant said. It is a noble organization. Some one came in and as the door opened I could see the snow falling. There will be no more offensive now that the snow has come, I said. Certainly not, said the major. You should go on leave. You should go to Rome, Naples, Sicily  He should visit Amalfi, said the lieutenant I will write you cards to my family in Amalfi. They will love you like a son. He should go to Palermo. He ought to go to Capri. I would like you to see Abruzzi and visit my family at Capracotta, said the priest. Listen to him talk about the Abruzzi. There's more snow there than here. He doesn't want to see peasants. Let him go to centres of culture and civilization. He should have fine girls. I will give you the ad- dresses of places in Naples. Beautiful young girls — ac- companied by their mothers. Ha ! Ha ! Ha ! The cap- tain spread his hand open, the thumb up and fingers outspread as when you make shadow pictures. There was a shadow from his hand on the wall He spoke again in pidgin Italian. You go away like this, he A FAREWELL TO ARMS 9 pointed to the thumb, and come back like this, he touched the little finger. Every one laughed. Look, said the captain. He spread the hand again. Again the candle-light made its shadows on the wall. He started with the upright thumb and named in their order the thumb and four fingers, soto-tenente (the thumb), tenente (first finger), capitano (next finger), maggiore (next to the little finger), and tenente-colo- nello (the little finger). You go away soto-tenente! You come back soto-colonello ! They all laughed. The captain was having a great success with finger games. He looked at the priest and shouted, Every night priest five against one ! They all laughed again. You must go on leave at once, the major said. I would like to go with you and show you things, the lieutenant said. When you come back bring a phonograph. Bring good opera disks. Bring Caruso. Don't bring Caruso. He bellows. Don't you wish you could bellow like him? He bellows. I say he bellows ! I would like you to go to Abruzzi, the priest said. The others were shouting. There is good hunting. You would like the people and though it is cold it is clear and dry. You could stay with my family. My father is a famous hunter. Come on, said the captain. We go whorehouse be- fore it shuts. Good night, I said to the priest. Good night, he said. CHAPTER III When I came back to the front we still lived in that town. There were many more guns in the country around and the spring had come. The fields were green and there were small green shoots on the vines, the trees along the road had small leaves and a breeze came from the sea. I saw the town with the hill and the old castle above it in a cup in the hills with the mountains beyond, brown mountains with a little green on their slopes. In the town there were more guns, there were some new hospitals, you met British men and sometimes women, on the street, and a few more houses had been hit by shell fire. It was warm and like the spring and I walked down the alleyway of trees, warmed from the sun on the wall, and found we still lived in the same house and that it all looked the same as when I had left it. The door was open, there was a soldier sitting on a bench outside in the sun, an ambulance was waiting by the side door and inside the door, as I went in, there was the smell of marble floors and hospital. It was all as I had left it except that now it was spring. I looked in the door of the big room and saw the major sitting at his desk, the window open and the sunlight coming into the room. He did not see me and I did not know whether to go in and report or go upstairs first and clean up. I decided to go on upstairs. The room I shared with the lieutenant Rinaldi looked out on the courtyard. The window was open, my bed was made up with blankets and my things hung on the wall, the gas mask in an oblong tin can, the steel helmet on the same peg. At the foot of the bed was my flat IO A FAREWELL TO ARMS u trunk, and my winter boots, the leather shiny with oil, were on the trunk. My Austrian sniper's rifle with its blued octagon barrel and the lovely dark walnut, cheek- fitted, schutzen stock, hung over the two beds. The telescope that fitted it was, I remembered, locked in the trunk. The lieutenant, Rinaldi, lay asleep on the other bed. He woke when he heard me in the room and sat up. Ciaou! he said. What kind of time did you have? Magnificent. We shook hands and he put his arm around my neck and kissed me. Oughf, I said. You're dirty, he said. You ought to wash. Where did you go and what did you do ? Tell me every- thing at once. I went everywhere. Milan, Florence, Rome, Naples, Villa San Giovanni, Messina, Taormina  You talk like a time-table. Did you have any beau- tiful adventures? Yes. Where? Milano, Firenze, Roma, Napoli  That's enough. Tell me really what was the best. In Milano. That was because it was first. Where did you meet her ? In the Cova ? Where did you go ? How did you feel? Tell me everything at once. Did you stay all night? Yes. That's nothing. Here now we have beautiful girls. New girls never been to the front before. Wonderful. 12 A FAREWELL TO ARMS You don't believe me? We will go now this after- noon and see. And in the town we have beautiful English girls. I am now in love with Miss Barkley. I will take you to call. I will probably marry Miss Barkley. I have to get washed and report. Doesn't anybody work now? Since you are gone we have nothing but frostbites, chilblains, jaundice, gonorrhea, self-inflicted wounds, pneumonia and hard and soft chancres. Every week some one gets wounded by rock fragments. There are a few real wounded. Next week the war starts again. Perhaps it starts again. They say so. Do you think I would do right to marry Miss Barkley — after the war of course? Absolutely, I said and poured the basin full of water. To-night you will tell me everything, said Rinaldi. Now I must go back to sleep to be fresh and beautiful for Miss Barkley. I took off my tunic and shirt and washed in the cold water in the basin. While I rubbed myself with a towel I looked around the room and out the window and at Rinaldi lying with his eyes closed on the bed. He was good-looking, was my age, and he came from Amalfi. He loved being a surgeon and we were great friends. While I was looking at him he opened his eyes. Have you any money? Yes. Loan me fifty lire. I dried my hands and took out my pocket-book from the inside of my tunic hanging on the wall. Rinaldi took the note, folded it without rising from the bed and slid it in his breeches pocket. He smiled, I must make A FAREWELL TO ARMS 13 on Miss Barkley the impression of a man of sufficient wealth. You are my great and good friend and finan- cial protector. Go to hell, I said. That night at the mess I sat next to the priest and he was disappointed and suddenly hurt that I had not gone to the Abruzzi. He had written to his father that I was coming and they had made preparations. I myself felt as badly as he did and could not understand why I had not gone. It was what I had wanted to do and I tried to explain how one thing had led to another and finally he saw it and understood that I had really wanted to go and it was almost all right. I had drunk much wine and afterward coffee and Strega and I explained, wine- fully, how we did not do the things we wanted to do ; we never did such things. We two were talking while the others argued. I had wanted to go to Abruzzi. I had gone to no place where the roads were frozen and hard as iron, where it was clear cold and dry and the snow was dry and powdery and hare-tracks in the snow and the peasants took off their hats and called you Lord and there was good hunt- ing. I had gone to no such place but to the smoke of cafes and nights when the room whirled and you needed to look at the wall to make it stop, nights in bed, drunk, when you knew that that was all there was, and the strange excitement of waking and not knowing who it was with you, and the world all unreal in the dark and so exciting that you must resume again unknowing and not caring in the night, sure that this was all and all and all and not caring. Suddenly to care very much and to sleep to wake with it sometimes morning and all that had been there gone and everything sharp and hard and clear and sometimes a dispute about the cost. i 4 A FAREWELL TO ARMS Sometimes still pleasant and fond and warm and break- fast and lunch. Sometimes all niceness gone and glad to get out on the street but always another day starting and then another night. I tried to tell about the night and the difference between the night and the day and how the night was better unless the day was very clean and cold and I could not tell it ; as I cannot tell it now. But if you have had it you know. He had not had it but he understood that I had really wanted to go to the Abruzzi but had not gone and we were still friends, with many tastes alike, but with the difference between us. He had always known what I did not know and what, when I learned it, I was always able to forget. But I did not know that then, although I learned it later. In the meantime we were all at the mess, the meal was finished, and the argument went on. We two stopped talking and the captain shouted, Priest not happy. Priest not happy without girls. I am happy, said the priest. Priest not happy. Priest wants Austrians to win the war, the captain said. The others listened. The priest shook his head. No, he said. Priest wants us never to attack. Don't you want us never to attack? No. If there is a war I suppose we must attack. Must attack. Shall attack ! The priest nodded. Leave him alone, the major said. He's all right. He can't do anything about it anyway, the captain said. We all got up and left the table. CHAPTER IV The battery in the next garden woke me in the morning and I saw the sun coming through the window and got out of the bed. I went to the window and looked out. The gravel paths were moist and the grass was wet with dew. The battery fired twice and the air came each time like a blow and shook the window and made the front of my pajamas flap. I could not see the guns but they were evidently firing directly over us. It was a nuisance to have them there but it was a com- fort that they were no bigger. As I looked out at the garden I heard a motor truck starting on the road. I dressed, went downstairs, had some coffee in the kitchen and went out to the garage. Ten cars were lined up side by side under the long shed. They were top-heavy, blunt-nosed ambulances, painted gray and built like moving-vans. The mechan- ics were working on one out in the yard. Three others were up in the mountains at dressing-stations. Do they ever shell that battery? I asked one of the mechanics. No, Signor Tenente. It is protected by the little hill. How's everything? Not so bad. This machine is no good but the others march. He stopped working and smiled. Were you on permission? Yes. He wiped his hands on his jumper and grinned. You have a good time? The others all grinned too. 15 16 A FAREWELL TO ARMS Fine/ 1 I said. What's the matter with this ma- chine ? It's no good. One thing after another. What's the matter now? New rings. I left them working, the car looking disgraced and empty with the engine open and parts spread on the work bench, and went in under the shed and looked at each of the cars. They were moderately clean, a few freshly washed, the others dusty. I looked at the tires carefully, looking for cuts or stone bruises. Every- thing seemed in good condition. It evidently made no difference whether I was there to look after things or not. I had imagined that the condition of the cars, whether or not things were obtainable, the smooth func- tioning of the business of removing wounded and sick from the dressing stations, hauling them back from the mountains to the clearing station and then distributing them to the hospitals named on their papers, depended to a considerable extent on myself. Evidently it did not matter whether I was there or not. Has there been any trouble getting parts?s I asked the sergeant mechanic. No, Signor Tenente. Where is the gasoKne park now? At the same place. Good, I said and went back to the house and drank another bowl of coffee at the mess table. The coffee was a pale gray and sweet with condensed milk. Outside the window it was a lovely spring morning. There was that beginning of a feeling of dryness in the nose that meant the day would be hot later on. That day I visited the posts in the mountains and was back in town late in the afternoon. A FAREWELL TO ARMS 17 The whole thing seemed to run better while I was away. The offensive was going to start again I heard. The division for which we worked were to attack at a place up the river and the major told me that I would see about the posts for during the attack. The attack would cross the river up above the narrow gorge and spread up the hillside. The posts for the cars would have to be as near the river as they could get and keep covered. They would, of course, be selected by the infantry but we were supposed to work it out. It was one of those things that gave you a false feeling of soldiering. I was very dusty and dirty and went up to my room to wash. Rinaldi was sitting on the bed with a copy of Hugo's English grammar. He was dressed, wore his black boots, and his hair shone. Splendid, he said when he saw me. You will come with me to see Miss Barkley. No. Yes. You will please come and make me a good im- pression on her. All right. Wait till I get cleaned up. Wash up and come as you are. I washed, brushed my hair and we started. Wait a minute, Rinaldi said. Perhaps we should have a drink. He opened his trunk and took out a bottle. Not Strega, I said. No. Grappa. All right. He poured two glasses and we touched them, first fingers extended. The grappa was very strong. Another? All right, I said. We drank the second grappa, Rinaldi put away the bottle and we went down the 18 A FAREWELL TO ARMS stairs. It was hot walking through the town but the sun was starting to go down and it was very pleasant. The British hospital was a big villa built by Germans before the war. Miss Barkley was in the garden. Another nurse was with her. We saw their white uniforms through the trees and walked toward them. Rinaldi saluted. I saluted too but more moderately. How do you do? Miss Barkley said.  You' re not an Italian, are you ? Oh, no. Rinaldi was talking with the other nurse. They were laughing. What an odd thing — to be in the Italian army. It's not really the army. It's only the ambulance. It's very odd though. Why did you do it? I don't know, I said. There isn't always an ex- planation for everything. Oh, isn't there? I was brought up to think there was. That's awfully nice. Do we have to go on and talk this way? No, I said. That's a relief. Isn't it? What is the stick? I asked. Miss Barkley was quite tall. She wore what seemed to me to be a nurse's uniform, was blonde and had a tawny skin and gray eyes. I thought she was very beautiful. She was carry- ing a thin rattan stick like a toy riding-crop, bound in leather. It belonged to a boy who was killed last year. I'm awfully sorry. He was a very nice boy. He was going to marry me and he was killed in the Somme. It was a ghastly show. A FAREWELL TO ARMS 19 Were you there ? No. I've heard about it, she said. There's not really any war of that sort down here. They sent me the little stick. His mother sent it to me. They returned it with his things. Had you been engaged long? Eight years. We grew up together. And why didn't you marry? I don't know, she said. I was a fool not to. I could have given him that anyway. But I thought it would be bad for him. 1 see. Have you ever loved any one? No, I said We sat down on a bench and I looked at her. You have beautiful hair, I said. Do you like it? Very much. I was going to cut it all off when he died. No. I wanted to do something for him. You see I didn't care about the other thing and he could have had it all. He could have had anything he wanted if I would have known. I would have married him or any- thing. I know all about it now. But then he wanted to go to war and I didn't know. I did not say anything. I didn't know about anything then. I thought it would be worse for him. I thought perhaps he couldn't stand it and then of course he was killed and that was the end of it. I don't know. Oh, yes, she said. That's the end of it. 20 A FAREWELL TO ARMS We looked at Rinaldi talking with the other nurse. What is her name? Ferguson. Helen Ferguson. Your friend is a doc- tor, isn't he? Yes. He's very good. That's splendid You rarely find any one any good this close to the front. This is close to the front, isn't it? Quite. It's a silly front, she said. But it's very beauti- ful. Are they going to have an offensive? Yes. Then we'll have to work. There's no work now. Have you done nursing long? Since the end of fifteen. I started when he did. I remember having a silly idea he might come to the hospital where I was. With a sabre cut, I suppose, and a bandage around his head. Or shot through the shoul- der. Something picturesque. This is the picturesque front, I said. Yes, she said. People can't realize what France is like. If they did, it couldn't all go on. He didn't have a sabre cut. They blew him all to bits. I didn't say anything. Do you suppose it will always go on? No. What's to stop it? It will crack somewhere. We'll crack. We'll crack in France. They can't go on doing things like the Somme and not crack. They won't crack here, I said. You think not? No. They did very well last summer. They may crack, she said. Anybody may crack. A FAREWELL TO ARMS 21 The Germans too. No, she said. I think not. We went over toward Rinaldi and Miss Ferguson. You love Italy? Rinaldi asked Miss Ferguson in English. Quite well. No understand, Rinaldi shook his head. Abbastanza bene, I translated. He shook his head. That is not good. You love England? Not too well. I'm Scotch, you see. Rinaldi looked at me blankly. She's Scotch, so she loves Scotland better than Eng- land, I said in Italian. But Scotland is England. I translated this for Miss Ferguson. Pas encore, said Miss Ferguson. Not really? Never. We do not like the English. Not like the English? Not like Miss Barkley? Oh, that's different. You mustn't take everything so literally. After a while we said good-night and left. Walking home Rinaldi said, Miss Barkley prefers you to me. That is very clear. But the little Scotch one is very nice. Very, I said. I had not noticed her. You like her? No, said Rinaldi. CHAPTER V The next afternoon I went to call on Miss Barkley again. She was not in the garden and I went to the side door of the villa where the ambulances drove up. Inside I saw the head nurse, who said Miss Barkley was on duty — there's a war on, you know. I said I knew. You're the American in the Italian army? she asked. Yes, ma'am. How did you happen to do that? Why didn't you join up with us ? I don't know, I said. Could I join now? I'm afraid not now. Tell me. Why did you join up with the Italians? I was in Italy, I said, and I spoke Italian. Oh, she said. I'm learning it It's a beautiful language. Somebody said you should be able to learn it in two weeks. Oh, I'll not learn it in two weeks. I've studied it for months now. You may come and see her after seven o'clock if you wish. She'll be off then. But don't bring a lot of Italians. Not even for the beautiful language? No. Nor for the beautiful uniforms. Good evening, I said. A rivederci, Tenente. A rivederla. I saluted and went out. It was im- possible to salute foreigners as an Italian, without em- A FAREWELL TO ARMS 23 barrassment The Italian salute never seemed made for export. The day had been hot. I had been up the river to the bridgehead at Plava. It was there that the offensive was to begin. It had been impossible to advance on the far side the year before because there was only one road leading down from the pass to the pontoon bridge and it was under machine-gun and shell fire for nearly a mile. It was not wide enough either to carry all the trans- port for an offensive and the Austrians could make a shambles out of it. But the Italians had crossed and spread out a little way on the far side to hold about a mile and a half on the Austrian side of the river. It was a nasty place and the Austrians should not have let them hold it. I suppose it was mutual tolerance because the Austrians still kept a bridgehead further down the river. The Austrian trenches were above on the hillside only a few yards from the Italian lines. There had been a little town but it was all rubble. There was what was left of a railway station and a smashed permanent bridge that could not be repaired and used because it was in plain sight. I went along the narrow road down toward the river, left the car at the dressing station under the hill, crossed the pontoon bridge, which was protected by a shoulder of the mountain, and went through the trenches in the smashed-down town and along the edge of the slope. Everybody was in the dugouts. There were racks of rockets standing to be touched oflf to call for help from the artillery or to signal with if the tele- phone wires were cut. It was quiet, hot and dirty. I looked across the wire at the Austrian lines. Nobody was in sight. I had a drink with a captain that I knew in one of the dugouts and went back across the bridge. 24 A FAREWELL TO ARMS A new wide road was being finished that would go over the mountain and zig-zag down to the bridge. When this road was finished the offensive would start. It came down through the forest in sharp turns. The system was to bring everything down the new road and take the empty trucks, carts and loaded ambulances and all returning traffic up the old narrow road. The dress- ing station was on the Austrian side of the river under the edge of the hill and stretcher-bearers would bring the wounded back across the pontoon bridge. It would be the same when the offensive started. As far as I could make out the last mile or so of the new road where it started to level out would be able to be shelled steadily by the Austrians. It looked as though it might be a mess. But I found a place where the cars would be sheltered after they had passed that last bad-looking bit and could wait for the wounded to be brought across the pontoon bridge. I would have liked to drive over the new road but it was not yet finished. It looked wide and well made with a good grade and the turns looked very impressive where you could see them through openings in the forest on the mountain side. The cars would be all right with their good metal-to-metal brakes and anyway, coming down, they would not be loaded. I drove back up the narrow road. Two carabinieri held the car up. A shell had fallen and while we waited three others fell up the road. They were seventy-sevens and came with a whishing rush of air, a hard bright burst and flash and then gray smoke that blew across the road. The carabinieri waved us to go on. Passing where the shells had landed I avoided the small broken places and smelled the high explosive and the smell of blasted clay and stone and freshly shattered flint. I drove back to Gorizia and our villa"

init : Model
init = { markovChain = trainMarkovChain <| tokenizeData initTrainingData
       , memory = 1
       , trainingData = initTrainingData
       , data = ""
       , seed = Random.initialSeed 0
       , errorMsg = Nothing
       }

suggestionStyle = [ ("backgroundColor", "#64908A")
                  , ("borderRadius", "3px")
                  , ("display", "inline-block")
                  , ("padding", "5px")
                  , ("margin", "3px")
                  , ("fontSize", "14px")
                  ]

viewSuggestion : Signal.Address Action -> State -> Html.Html
viewSuggestion address state = div [style suggestionStyle, onClick address (TakeSuggestion state) ] [ text state ]

greeting = """
# Here lies an: Interactive Markov Chain Based Text Suggester

On the right we have the data used to train our Markov chain, feel free to paste in your own writing samples, and on the left we have an area for you to type.

  * To take the top suggestion, press **Tab** *(you can also click on the suggestion)*
  * You can also have the machines do your work, try hitting the **daydream** button

###### Some notes for david:
  * The Markov chain is retrained everytime the training data changes, should make that clearer
  * Try retraining with the input data as well
"""

view : Signal.Address Action -> Model -> Html.Html
view address model =
  let styles =
        [ ("height", "100%")
        , ("width", "100%")
        , ("position", "absolute")
        , ("backgroundColor", "#ffffff") ]
      
      inputStyle =
        [ ("width", "47%")
        , ("height", "500px")
        , ("padding", "10px")
        , ("border", "none")
        , ("display", "inline-block")
        , ("resize", "none")
        , ("fontSize", "14px")
        , ("position", "absolute")
        , ("borderRadius", "4px")
        ]
      userInputStyle = inputStyle ++ [ ("left", "1%")
                                     , ("backgroundColor", "#E8DDA4")
                                     , ("color", "#222222") ]
      trainingInputStyle = inputStyle ++ [ ("right", "1%")
                                         , ("backgroundColor", "#424254") 
                                         , ("color", "#dddddd") ]
      daydreamButtonStyle = suggestionStyle ++ [ ("backgroundColor", "#351330")
                                               , ("color", "#dddddd")
                                               , ("borderStyle", "none") ]
      errorString = case model.errorMsg of
                      Nothing -> ""
                      Just s  -> s
      errorStyle = suggestionStyle ++ [ ("backgroundColor", "#CC2A41")
                                      , ("display", if model.errorMsg == Nothing then "none" else "inline-block") ]
  in 
    div [style styles]
          [ div [style [ ("display", "inline-block")
                       , ("margin", "1%") ] ]
                  [ Markdown.toHtml greeting
                  , button [ style daydreamButtonStyle, onClick address Daydream ] [ text "Daydream" ]
                  , div [style errorStyle] [ text errorString ]
                  , div [style [("display", "inline-block")]] <| List.map (viewSuggestion address) <| topSuggestions 10 model
                  ]
          , br [] []
          , textarea [ style userInputStyle, tabindex -1, onInput address Input, onTab address TakeTopSuggestion, value model.data] []
          , textarea [ style trainingInputStyle, onInput address TrainingDataInput, value model.trainingData] []
    ]

preventDefaultOptions : Options
preventDefaultOptions = { defaultOptions | preventDefault <- True }

tabDecoder : Json.Decode.Decoder Int
tabDecoder = customDecoder keyCode (\code -> case code of
                                               9 -> Result.Ok code
                                               _ -> Result.Err "Not a tab")

onTab : Signal.Address a -> a -> Attribute
onTab address action = onWithOptions "keydown" preventDefaultOptions tabDecoder (\_ -> Signal.message address action)

onInput : Signal.Address a -> (String -> a) -> Attribute
onInput address contentToValue = on "input" targetValue (\str -> Signal.message address (contentToValue str))
      
type Action = NoOp
            | TrainMarkovChain
            | Daydream
            | Input String
            | TrainingDataInput String
            | TakeTopSuggestion
            | TakeSuggestion State

tokenSeperator a b =
    if endsInSpace a || a == initState then
      ""
    else
      " "
    
              
appendState : String -> State -> String
appendState data state =
  let
    curState = currentState data
    newData = if | endsInSpace data -> data
                 | otherwise -> String.slice 0 -(String.length curState) data

    data' = String.concat [newData, tokenSeperator newData state, state, " "]
  in
    data'
  
            
handleAction : Action -> Model -> Model
handleAction action model =  
    case action of
      NoOp -> model
      TakeSuggestion state ->
        { model | data <- appendState model.data state }
      TakeTopSuggestion ->
        let
          data' = case topSuggestion model of
                    Just state -> appendState model.data state
                    Nothing -> model.data
        in
          { model | data <- data' }
      Input string             -> { model | data <- string }
      TrainingDataInput string -> { model | trainingData <- string
                                          , markovChain <- trainMarkovChain (tokenizeData string) }
      TrainMarkovChain         -> { model | markovChain <- trainMarkovChain (tokenizeData model.trainingData) }
      Daydream                 ->
        let
          predictState = predictionState model.data
        in
          case nextState model.seed model.markovChain predictState of
            (Just token, newSeed) -> { model | data <- appendState model.data token
                                             , seed <- newSeed}
            (Nothing, newSeed)    -> { model | seed <- newSeed}

updateErrorMsg : Model -> Model
updateErrorMsg model =
  case topSuggestion model of
    Just _  -> { model | errorMsg <- Nothing }
    Nothing ->
      let
        msg = if previousState model.data == initState then
                String.concat [ "Interesting, I've never seen someone start with '"
                              , currentState model.data
                              , "'"
                              ]
              else 
                let
                  problemState = nStatesBack 0 model.data
                  prevState = previousState model.data
                  problem = String.concat [ prevState
                                          , tokenSeperator prevState problemState
                                          , problemState ]
                in
                  "I'm Stuck! feed me some examples that start with '" ++ problem ++ "'"
      in
        { model | errorMsg <- Just msg }

update : Action -> Model -> Model
update action = handleAction action >> updateErrorMsg

updateNextState : State -> Maybe (List (Float, State)) -> Maybe (List (Float, State))
updateNextState nextState maybeNextStates =
  case maybeNextStates of
    Nothing         -> Just [(1, nextState)]
    Just nextStates ->
      case List.filter (\(_, s) -> s == nextState) nextStates of
        []       ->
          Just <| (1, nextState) :: nextStates
        [(p, s)] as states ->
          let
            statesLen = toFloat <| List.length states
            newP = (p * statesLen + 1) / (statesLen + 1)
            otherStates = nextStates
                        |> List.filter (\(_, s) -> s /= nextState) 
                        |> List.map (\(p, s) -> ((p * statesLen) / (statesLen + 1), s))
            
          in
            Just <| (newP, nextState) :: otherStates
        otherwise -> Nothing -- This shouldn't happen, something went wrong

updateState : State -> State -> MarkovChain -> MarkovChain
updateState state nextState mc = Dict.update state (updateNextState nextState) mc

trainMarkovChain : List State -> MarkovChain
trainMarkovChain input =
  let
    (prev, markovChain) = List.foldl (\state (p, mc) -> (state, updateState p state mc)) (initState, Dict.empty) input
  in
    markovChain

tokenizeData : String -> List State
tokenizeData data = List.filter (\token -> token /= "") <| String.words data

probability : Random.Generator Float
probability = Random.float 0 1

-- essentially computes the integral of the probability distribution
-- e.g. [(0.1, a), (0.5, b), (0.3, c), (0.1, d)] -> [(0.1, a), (0.6, b), (0.9, c), (1, d)]
-- This makes drawing a random state weighted by this probability distribution much easier.
-- Just generate 0 < p < 1 and take the first state where cumulative p value is greater than p

cumulativePValues : List (Float, State) -> List (Float, State)
cumulativePValues nextStates =
  let
    f (p, state) ((accumP, _) :: _ as past) = (p + accumP, state)::past
    cumulativeStates = List.foldl f [(0, initState)] nextStates
  in
    List.reverse cumulativeStates

suggestionsForState : MarkovChain -> State -> List State
suggestionsForState mc state =
  case Dict.get state mc of
    Nothing -> []
    Just nextStates ->           -- nextStates is a list of (p value, state)
      List.sortBy fst nextStates -- sort by the p values
        |> List.reverse          -- we want highest p values first
        |> List.map snd          -- drop the p values and only keep the states

topSuggestion : Model -> Maybe State   
topSuggestion model = List.head <| suggestions model

topSuggestions : Int -> Model -> List State
topSuggestions n model = List.take n <| suggestions model

endsInSpace : String -> Bool
endsInSpace = String.endsWith " "
                         
suggestions : Model -> List State
suggestions model =
  let predictState = predictionState model.data
      curState = currentState model.data
      allSuggestions = suggestionsForState model.markovChain predictState
  in List.filter (String.startsWith curState) allSuggestions
      
nextState : Random.Seed -> MarkovChain -> State -> (Maybe State, Random.Seed)
nextState seed mc state =
  case Dict.get state mc of
    Nothing -> (Nothing, seed)
    Just nextStates ->
      let
        (p, seed') = Random.generate probability seed
        (nextState, seed'')  = -- the first state whose commulative p value is greater or equal to the random p value 'p'
          cumulativePValues nextStates 
            |> List.filter (\(cumulativeP, _) -> cumulativeP >= p)
            |> (\l -> let (n, seed'') = Random.generate (Random.int 0 (List.length l - 1)) seed'
                      in (List.head <| List.drop n l, seed''))
      in
        case nextState of
          Nothing      -> (Nothing, seed'') -- shouldn't ever happen, but w/e
          Just (_, ns) -> (Just ns, seed'')

currentState : String -> State
currentState data =
  if endsInSpace data then
    initState
  else
    nStatesBack 0 data

predictionState : String -> State
predictionState data =
  if endsInSpace data then
    nStatesBack 0 data
  else
    nStatesBack 1 data

previousState : String -> State
previousState data = nStatesBack 1 data

nStatesBack : Int -> String -> State
nStatesBack n data = case List.head <| List.drop n <| List.reverse <| tokenizeData data of
                       Nothing    -> initState
                       Just state -> state

actions : Signal.Mailbox Action
actions = Signal.mailbox NoOp

modelSignal : Signal Model
modelSignal = Signal.foldp update init actions.signal

main : Signal Html.Html
main = Signal.map (view actions.address) modelSignal
