#![cfg_attr(not(feature = "std"), no_std)]

pub use pallet::*;

#[frame_support::pallet]
pub mod pallet {
    use frame_support::{
        sp_runtime::traits::{Hash, Zero},
        dispatch::{DispatchResultWithPostInfo, DispatchResult},
        traits::{Currency, ExistenceRequirement, Randomness},
        pallet_prelude::*,transactional
    };
    use frame_system::pallet_prelude::*;
    use sp_io::hashing::blake2_128;
    #[cfg(feature = "std")]
    use frame_support::serde::{Deserialize, Serialize};


    #[derive(Clone, Encode, Decode, PartialEq, RuntimeDebug, TypeInfo)]
    #[scale_info(skip_type_params(T))]  
    pub struct Kitty<T: Config> {
        dna: [u8; 16],
        gender: Gender,
        price: Option<<T::Currency as Currency<T::AccountId>>::Balance>,
        owner: T::AccountId
        
    }
    #[derive(Clone, Encode, Decode, PartialEq, RuntimeDebug, TypeInfo)]
    #[scale_info(skip_type_params(T))]
    #[cfg_attr(feature = "std", derive(Serialize, Deserialize))]

    pub enum Gender {
        Male,
        Female
    }





    #[pallet::pallet]
    #[pallet::generate_store(pub(super) trait Store)]
    pub struct Pallet<T>(_);

    /// Configure the pallet by specifying the parameters and types it depends on.
    #[pallet::config]
    pub trait Config: frame_system::Config {
        /// Because this pallet emits events, it depends on the runtime's definition of an event.
        type Event: From<Event<Self>> + IsType<<Self as frame_system::Config>::Event>;
        /// The Currency handler for the Kitties pallet.
        type Currency: Currency<Self::AccountId>;

        type KittyRand: Randomness<Self::Hash, Self::BlockNumber>;
        // All constants defined in the runtime implement Get<T> trait, created by using parameter_type! in runtime
        type MaxKittyOwned: Get<u32>;

    }




    // Errors.
    #[pallet::error]
    pub enum Error<T> {
        OwnTooMany,
        MaxKittyReached,
        NotOwner,
        NoKittyFound,
        SelfTransfer,
        NotForSale,
        CannotAfford,
        LowBid,
    }






    #[pallet::event]
    #[pallet::generate_deposit(pub(super) fn deposit_event)]
    pub enum Event<T: Config>{
        KittyCreated(T::AccountId, T::Hash),
        PriceSet(T::Hash, Option<<T::Currency as Currency<T::AccountId>>::Balance>),
        KittyTransferred(T::AccountId, T::Hash, T::AccountId),
    }






    #[pallet::storage]
    #[pallet::getter(fn kitty_count)]
    pub(super) type KittyCount<T: Config> = StorageValue<_, u64, ValueQuery>;


    #[pallet::storage]
    #[pallet::getter(fn kitties)]
    // Notice this doesnt take a ValueQuery, it it given the default OptionQuery, so if a given hash
    // does not have a kitty it returns none.
    pub(super) type Kitties<T: Config> = StorageMap<
        _,
        Twox64Concat,
        T::Hash,
        Kitty<T>,
    >;


    #[pallet::storage]
    #[pallet::getter(fn kitties_owned)]
    /// Keeps track of what accounts own what Kitty.
    pub(super) type KittiesOwned<T: Config> = StorageMap<
        _,
        Twox64Concat,
        T::AccountId,
        BoundedVec<T::Hash, T::MaxKittyOwned>,
        ValueQuery,
    >;





    #[pallet::call]
    impl<T: Config> Pallet<T> {

        #[pallet::weight(100)]
        pub fn transfer(origin: OriginFor<T>, to: T::AccountId, id: T::Hash) -> DispatchResult {
            let sender = ensure_signed(origin)?;
            let owner_of_kitty = Self::owner_of(&id)?;
            // Simple checks that we use regurarly, if you are familair with solidity, this is analagous to 
            // the standard require statements checking against various things such as the 0 address.
            ensure!(sender == owner_of_kitty, Error::<T>::NotOwner);
            ensure!(sender != to, Error::<T>::SelfTransfer);
            ensure!((KittiesOwned::<T>::get(&to).len() as u32) < T::MaxKittyOwned::get(), Error::<T>::MaxKittyReached);

            Self::transfer_kitty_to(&to, &id, &sender)?;
            Self::deposit_event(Event::<T>::KittyTransferred(to, id, sender));

    
            Ok(())
        }


        #[pallet::weight(100)]
        pub fn create_kitty(origin: OriginFor<T>) -> DispatchResult {
            let sender = ensure_signed(origin)?;

            let id = Self::mint(&sender, None, None)?;
            log::info!("Kitty: {:?} created by: {:?}", id, sender);
            Self::deposit_event(Event::<T>::KittyCreated(sender, id));
			
            Ok(())
        }


        #[pallet::weight(100)]
        pub fn set_price(origin: OriginFor<T>, price: Option<<T::Currency as Currency::<T::AccountId>>::Balance>, id: T::Hash) -> DispatchResult {
            let sender = ensure_signed(origin)?;
            // Check to see if kitty exists + if sender is owner
            let owner = Self::owner_of(&id)?;
            ensure!(sender == owner, Error::<T>::NotOwner);
            
            let mut kitty = Kitties::<T>::get(&id).ok_or(Error::<T>::NoKittyFound)?;
            kitty.price = price.clone();
            Kitties::<T>::insert(&id, kitty);
            Self::deposit_event(Event::<T>::PriceSet(id, price));


            Ok(())
        }

        #[transactional]
        #[pallet::weight(100)]
        pub fn buy_kitty(origin: OriginFor<T>, id: T::Hash, bid_price: <T::Currency as Currency<T::AccountId>>::Balance) -> DispatchResult {
            let sender = ensure_signed(origin)?;

            let kitty = Kitties::<T>::get(&id).ok_or(Error::<T>::NoKittyFound)?;
            let buying_price = Self::for_sale(&kitty).map_err(|_| Error::<T>::NotForSale)?;
            let sender_vec = KittiesOwned::<T>::get(&sender);

            ensure!(bid_price >= buying_price, Error::<T>::LowBid);
            ensure!(<T::Currency as Currency::<T::AccountId>>::free_balance(&sender) >= bid_price, Error::<T>::CannotAfford);
            ensure!(sender_vec.len() < (T::MaxKittyOwned::get() as usize), Error::<T>::OwnTooMany);

            T::Currency::transfer(&sender, &kitty.owner, bid_price, ExistenceRequirement::KeepAlive)?;
            Self::transfer_kitty_to(&sender, &id, &kitty.owner)?;
            Self::deposit_event(Event::<T>::KittyTransferred(sender, id, kitty.owner.clone()));


            Ok(())
        }

    }



    impl<T: Config> Pallet<T> {
        fn gen_gender() -> Gender {
            // random returns a tuple, we want the hash, not the block number, so use .0 to access the first item
            let rand = T::KittyRand::random(&b"gender"[..]).0;
            // Cannot index into a Hash which is a type required to be implemented by T (see system: Config),
            // so as_ref is needed which returns a reference to the inner type in the struct.
            match rand.as_ref()[0] % 2 {
                1 => Gender::Male,
                _ => Gender::Female
            }
        }

        fn gen_dna() -> [u8; 16] {
            let payload = (T::KittyRand::random(&b"DNA:"[..]).0, <frame_system::Pallet<T>>::block_number());
            payload.using_encoded(blake2_128)
        }

        // Given this is how kitties are minted, challenege yourself to exploit the system by creating a way the misaligns the kitty count
        // with the actual number of kitties. Find answer in ReadMe, but have a try first!
        pub fn mint(
            owner: &T::AccountId,
            dna: Option<[u8; 16]>,
            gender: Option<Gender>,
        ) -> Result<(T::Hash), Error<T>> {
            let kitty = Kitty::<T> {
                dna: dna.unwrap_or_else(Self::gen_dna),
                price: None ,
                gender: gender.unwrap_or_else(Self::gen_gender),
                owner: owner.clone(),
            };

            let kitty_id = T::Hashing::hash_of(&kitty);
            let new_count = Self::kitty_count().checked_add(1).ok_or(Error::<T>::MaxKittyReached)?;
            KittiesOwned::<T>::try_mutate(&owner, |kitty_vec| {
                kitty_vec.try_push(kitty_id)
            }).map_err(|_| {
                Error::<T>::OwnTooMany
            })?;
            KittyCount::<T>::set(new_count);
            Kitties::<T>::insert(kitty_id, kitty);


            Ok(kitty_id)
        }

        pub fn owner_of(id: &T::Hash) -> Result<T::AccountId, Error<T>> {
            let kitty = Kitties::<T>::get(id);
            let real_kitty = match kitty {
                Some(pur) => Ok(pur),
                None => Err(Error::<T>::NoKittyFound)
            }?;
            Ok(real_kitty.owner)
        }
        #[transactional]
        // Functions that use this transactional attribute must return a Result, otherwise compile error. If the function returns an
        // Err(), all changes to storage are reverted. E.g. if this func below returns an error because the receiver owns too many 
        // kitties, then all prior changes to storage are reverted.
        pub fn transfer_kitty_to(to: &T::AccountId, id: &T::Hash, from: &T::AccountId) -> Result<(), Error<T>> {
            let mut kitty = Kitties::<T>::get(id).ok_or(Error::<T>::NoKittyFound)?;
            
            // try_mutate is very powerful it takes a closure which returns a result, and only mutates the value if result is ok(),
            // it returns result so that if we get an error, we can propogate it back to the calling func and storage is not changed.
            KittiesOwned::<T>::try_mutate(from, |vec| {
                // See position docs
                if let Some(index) = vec.iter().position(|ind| {
                    *ind == *id
                }) {
                    //Swaps last item with item at index, then reduces len of vec by 1. 
                    vec.swap_remove(index);
                    return Ok(());
                } 
                    Err(())
                
            }).map_err(|_| Error::<T>::NoKittyFound)?;

            KittiesOwned::<T>::try_mutate(&to, |vec| {
                // No need to clone id since it is a hash which implements copy --> see system::Config for validation
                vec.try_push(*id)
            }).map_err(|_| Error::<T>::OwnTooMany)?;

            kitty.owner = to.clone();
            kitty.price = None;
            Kitties::<T>::insert(&id, kitty);

            Ok(())
        }

        pub fn for_sale(kitty: &Kitty<T>) -> Result<<T::Currency as Currency<T::AccountId>>::Balance, ()> {
            if kitty.price == None {
                return Err(())
            }
            return kitty.price.ok_or(())
        }
        



    }
}
