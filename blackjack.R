deck1=c(2:10)
deck2=c(10,10,10)
deck=c(deck1,deck2)
acevalue=function(hand){
    if(df_dealer[1,1]+df_dealer[1,2]<=10 && !is.na(df_dealer[1,1]+df_dealer[1,2]))
    {
      return(11)
    }
    else
    {
      return(1)
    }
  }
  
df_dealer[1,1]=deck[dealer_hand]
df_player=data.frame(
  first_card=c(00),
  second_card=c(00),
  Value=c(0),
  stringsAsFactors = FALSE
)
df_dealer=data.frame(
  Shown_Card=c(00),
  Hidden_Card=c(00),
  Value=c(0),
  stringsAsFactors = FALSE
)
dealer=0
player=0
for(j in 1:100)
{
for(i in 1:2)
{
dealer_hand=sample(1:13,1)
if(dealer_hand==13)
{
  card=acevalue(dealer_hand)
  df_dealer[j,i]=card
}
df_dealer[j,i]=deck[dealer_hand]
player_hand=sample(1:13,1)
if(player_hand==13)
{
  card=acevalue(player_hand)
  df_player[j,i]=card
}
df_player[j,i]=deck[player_hand]
}
player_sum=df_player[j,1]+df_player[j,2]
dealer_sum=df_dealer[j,1]+df_player[j,2]
df_dealer[j,3]=dealer_sum
df_player[j,3]=player_sum
if(player_sum>21 && !is.na(player_sum))
{
  dealer=dealer+1
  print("You Busted! Dealer Won")
}
if(dealer_sum>21 && !is.na(dealer_sum)){
  player=player+1
  print("Dealer Busted! You Won")
}
if(player_sum>dealer_sum && !is.na(player_sum>dealer_sum))
{
  player=player+1  
  print("You Won")
}else{
  dealer=dealer+1
  print("Dealer Won")
}
}
paste0("Player Winning Percentage:", player,"%")
paste0("Dealer Winning Percentage:", dealer,"%")