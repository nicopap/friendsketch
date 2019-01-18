use fxhash::FxHashMap;
use lazy_static::lazy_static;

#[rustfmt::skip]
pub static LIST: [&str;256] = [ "Abstract.", "Acetic.", "Aerial.", "Aged.", "Agricultural.", "Ambulant.", "Anchoral.", "Annual.", "Apatetic.", "Apian.", "Aquatic.", "Aqueous.", "Aquiline.", "Arcane.", "Aristocratic.", "Asinine.", "Astragalar.", "Astral.", "Auditory.", "Aural.", "Auric.", "Avian.", "Axillary.", "Benthic.", "Bolar.", "Bony.", "Boreal.", "Bovine.", "Bucolic.", "Caloric.", "Canadian.", "Canine.", "Carceral.", "Cardiac.", "Carnal.", "Carpal.", "Caudal.", "Celestial.", "Cenatory.", "Ceramic.", "Cerebral.", "Cerotic.", "Cervical.", "Cervine.", "Chelonian.", "Chinese.", "Cholic.", "Chthonic.", "Collateral.", "Commercial.", "Corvine.", "Costal.", "Creatic.", "Culinary.", "Daily.", "Delphine.", "Dental.", "Dermal.", "Devilish.", "Didactic.", "Digital.", "Diluvial.", "Diluvian.", "Divine.", "Domestic.", "Dorsal.", "Dutch.", "Ecclesiastic.", "Elapid.", "Encephalic.", "Eolian.", "Episcopal.", "Epistolary.", "Epithetic.", "Equine.", "Eristic.", "Estical.", "Estival.", "Eupeptic.", "Euxine.", "Feline.", "Ferrous.", "Filial.", "Final.", "Financial.", "Former.", "Formic.", "Fraternal.", "Fulmineous.", "Funebral.", "Fungous.", "Funicular.", "Galactic.", "Gelastic.", "Georgic.", "Geriatric.", "Gestic.", "Glacial.", "Glassy.", "Glavial.", "Gnathic.", "Gnomic.", "Golden.", "Good.", "Grassy.", "Gubernatorial.", "Gustatory.", "Haligonian.", "Haptic.", "Hellenic.", "Helvetic.", "Hermetic.", "Hippic.", "Hispanic.", "Hydric.", "Ichthyic.", "Icy.", "Iliac.", "Ilicic.", "Infantile.", "Initial.", "Insular.", "Juridical.", "Juvenile.", "Kinetic.", "Lagomorphic.", "Laic.", "Legal.", "Leonine.", "Lexical.", "Liminal.", "Littoral.", "Living.", "Loving.", "Lunar.", "Lupine.", "Malar.", "Malic.", "Mammary.", "Manual.", "Marine.", "Massive.", "Mechanical.", "Meline.", "Mental.", "Milky.", "Mimetic.", "Monetary.", "Motor.", "Nasal.", "Natal.", "Nautical.", "Naval.", "Nepotal.", "Nervous.", "Nimonic.", "Nominal.", "Northerly.", "Nuptial.", "Occidental.", "Occipital.", "Oceanic.", "Oily.", "Oleic.", "Olfactory.", "Oneiric.", "Ophic.", "Optic.", "Oriental.", "Osseous.", "Otic.", "Owlish.", "Parietal.", "Paschal.", "Passerine.", "Pastoral.", "Pecuniary.", "Pedagogical.", "Pedestrain.", "Philatelic.", "Physical.", "Plebeian.", "Pluvial.", "Pneumatic.", "Popular.", "Porcine.", "Priapean.", "Pteric.", "Ramal.", "Ranine.", "Rhematic.", "Rhizic.", "Royal.", "Rural.", "Russian.", "Rustic.", "Sagittal.", "Sartorial.", "Satanic.", "Sciatic.", "Semantic.", "Sensuous.", "Serpentine.", "Sideral.", "Simian.", "Sleepy.", "Solar.", "Somatic.", "Spectral.", "Specular.", "Stannic.", "Stellar.", "Sternal.", "Succinic.", "Sulphuric.", "Sunny.", "Superficial.", "Sylvan.", "Symbolic.", "Tactic.", "Tactile.", "Taurine.", "Technical.", "Tegular.", "Terminal.", "Theological.", "Thermal.", "Thesial.", "Thespian.", "Thionic.", "Tonetic.", "Tonsorial.", "Truthful.", "Uletic.", "Urban.", "Ursine.", "Uxorial.", "Vascular.", "Ventral.", "Veracious.", "Verbal.", "Vesical.", "Vespertilian.", "Vespine.", "Vinic.", "Visceral.", "Visual.", "Vital.", "Vulpine.", "Waxen.", "Weekly.", "Wolven.", "Yogic.", "Zibeline.", "Zoic.", "Zymic.", ];

lazy_static! {
    static ref FXHASH: FxHashMap<&'static str, u8> = {
        let mut m = FxHashMap::default();
        for (i, &name) in LIST.iter().enumerate() {
            m.insert(name, i as u8);
        }
        m
    };
}

pub fn from(adjective: &str) -> Option<u8> {
    FXHASH.get(&adjective).map(|&x| x)
}
