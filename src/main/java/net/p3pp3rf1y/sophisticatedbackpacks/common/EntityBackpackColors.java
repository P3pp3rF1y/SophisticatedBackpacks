package net.p3pp3rf1y.sophisticatedbackpacks.common;

import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.EntityTypes;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackWrapper;

import java.util.HashMap;
import java.util.Map;

public class EntityBackpackColors {
	private static final Map<EntityType<?>, BackpackColors> ENTITY_COLORS = new HashMap<>();
	static {
		ENTITY_COLORS.put(EntityTypes.ARMADILLO, new BackpackColors(11366765, 8538184));
		ENTITY_COLORS.put(EntityTypes.ALLAY, new BackpackColors(56063, 44543));
		ENTITY_COLORS.put(EntityTypes.AXOLOTL, new BackpackColors(16499171, 10890612));
		ENTITY_COLORS.put(EntityTypes.BAT, new BackpackColors(4996656, 986895));
		ENTITY_COLORS.put(EntityTypes.BEE, new BackpackColors(15582019, 4400155));
		ENTITY_COLORS.put(EntityTypes.BLAZE, new BackpackColors(16167425, 16775294));
		ENTITY_COLORS.put(EntityTypes.BOGGED, new BackpackColors(9084018, 3231003));
		ENTITY_COLORS.put(EntityTypes.BREEZE, new BackpackColors(11506911, 9529055));
		ENTITY_COLORS.put(EntityTypes.CAT, new BackpackColors(15714446, 9794134));
		ENTITY_COLORS.put(EntityTypes.CAMEL, new BackpackColors(16565097, 13341495));
		ENTITY_COLORS.put(EntityTypes.CAVE_SPIDER, new BackpackColors(803406, 11013646));
		ENTITY_COLORS.put(EntityTypes.CHICKEN, new BackpackColors(10592673, 16711680));
		ENTITY_COLORS.put(EntityTypes.COD, new BackpackColors(12691306, 15058059));
		ENTITY_COLORS.put(EntityTypes.COW, new BackpackColors(4470310, 10592673));
		ENTITY_COLORS.put(EntityTypes.CREEPER, new BackpackColors(894731, 0));
		ENTITY_COLORS.put(EntityTypes.DOLPHIN, new BackpackColors(2243405, 16382457));
		ENTITY_COLORS.put(EntityTypes.DONKEY, new BackpackColors(5457209, 8811878));
		ENTITY_COLORS.put(EntityTypes.DROWNED, new BackpackColors(9433559, 7969893));
		ENTITY_COLORS.put(EntityTypes.ELDER_GUARDIAN, new BackpackColors(13552826, 7632531));
		ENTITY_COLORS.put(EntityTypes.ENDER_DRAGON, new BackpackColors(1842204, 14711290));
		ENTITY_COLORS.put(EntityTypes.ENDERMAN, new BackpackColors(1447446, 0));
		ENTITY_COLORS.put(EntityTypes.ENDERMITE, new BackpackColors(1447446, 7237230));
		ENTITY_COLORS.put(EntityTypes.EVOKER, new BackpackColors(9804699, 1973274));
		ENTITY_COLORS.put(EntityTypes.FOX, new BackpackColors(14005919, 13396256));
		ENTITY_COLORS.put(EntityTypes.FROG, new BackpackColors(13661252, 16762748));
		ENTITY_COLORS.put(EntityTypes.GHAST, new BackpackColors(16382457, 12369084));
		ENTITY_COLORS.put(EntityTypes.GLOW_SQUID, new BackpackColors(611926, 8778172));
		ENTITY_COLORS.put(EntityTypes.GOAT, new BackpackColors(10851452, 5589310));
		ENTITY_COLORS.put(EntityTypes.GUARDIAN, new BackpackColors(5931634, 15826224));
		ENTITY_COLORS.put(EntityTypes.HOGLIN, new BackpackColors(13004373, 6251620));
		ENTITY_COLORS.put(EntityTypes.HORSE, new BackpackColors(12623485, 15656192));
		ENTITY_COLORS.put(EntityTypes.HUSK, new BackpackColors(7958625, 15125652));
		ENTITY_COLORS.put(EntityTypes.IRON_GOLEM, new BackpackColors(14405058, 7643954));
		ENTITY_COLORS.put(EntityTypes.LLAMA, new BackpackColors(12623485, 10051392));
		ENTITY_COLORS.put(EntityTypes.MAGMA_CUBE, new BackpackColors(3407872, 16579584));
		ENTITY_COLORS.put(EntityTypes.MOOSHROOM, new BackpackColors(10489616, 12040119));
		ENTITY_COLORS.put(EntityTypes.MULE, new BackpackColors(1769984, 5321501));
		ENTITY_COLORS.put(EntityTypes.OCELOT, new BackpackColors(15720061, 5653556));
		ENTITY_COLORS.put(EntityTypes.PANDA, new BackpackColors(15198183, 1776418));
		ENTITY_COLORS.put(EntityTypes.PARROT, new BackpackColors(894731, 16711680));
		ENTITY_COLORS.put(EntityTypes.PHANTOM, new BackpackColors(4411786, 8978176));
		ENTITY_COLORS.put(EntityTypes.PIG, new BackpackColors(15771042, 14377823));
		ENTITY_COLORS.put(EntityTypes.PIGLIN, new BackpackColors(10051392, 16380836));
		ENTITY_COLORS.put(EntityTypes.PIGLIN_BRUTE, new BackpackColors(5843472, 16380836));
		ENTITY_COLORS.put(EntityTypes.PILLAGER, new BackpackColors(5451574, 9804699));
		ENTITY_COLORS.put(EntityTypes.POLAR_BEAR, new BackpackColors(15658718, 14014157));
		ENTITY_COLORS.put(EntityTypes.PUFFERFISH, new BackpackColors(16167425, 3654642));
		ENTITY_COLORS.put(EntityTypes.RABBIT, new BackpackColors(10051392, 7555121));
		ENTITY_COLORS.put(EntityTypes.RAVAGER, new BackpackColors(7697520, 5984329));
		ENTITY_COLORS.put(EntityTypes.SALMON, new BackpackColors(10489616, 951412));
		ENTITY_COLORS.put(EntityTypes.SHEEP, new BackpackColors(15198183, 16758197));
		ENTITY_COLORS.put(EntityTypes.SHULKER, new BackpackColors(9725844, 5060690));
		ENTITY_COLORS.put(EntityTypes.SILVERFISH, new BackpackColors(7237230, 3158064));
		ENTITY_COLORS.put(EntityTypes.SKELETON, new BackpackColors(12698049, 4802889));
		ENTITY_COLORS.put(EntityTypes.SKELETON_HORSE, new BackpackColors(6842447, 15066584));
		ENTITY_COLORS.put(EntityTypes.SLIME, new BackpackColors(5349438, 8306542));
		ENTITY_COLORS.put(EntityTypes.SNIFFER, new BackpackColors(8855049, 2468720));
		ENTITY_COLORS.put(EntityTypes.SNOW_GOLEM, new BackpackColors(14283506, 8496292));
		ENTITY_COLORS.put(EntityTypes.SPIDER, new BackpackColors(3419431, 11013646));
		ENTITY_COLORS.put(EntityTypes.SQUID, new BackpackColors(2243405, 7375001));
		ENTITY_COLORS.put(EntityTypes.STRAY, new BackpackColors(6387319, 14543594));
		ENTITY_COLORS.put(EntityTypes.STRIDER, new BackpackColors(10236982, 5065037));
		ENTITY_COLORS.put(EntityTypes.TADPOLE, new BackpackColors(7164733, 1444352));
		ENTITY_COLORS.put(EntityTypes.TRADER_LLAMA, new BackpackColors(15377456, 4547222));
		ENTITY_COLORS.put(EntityTypes.TROPICAL_FISH, new BackpackColors(15690005, 16775663));
		ENTITY_COLORS.put(EntityTypes.TURTLE, new BackpackColors(15198183, 44975));
		ENTITY_COLORS.put(EntityTypes.VEX, new BackpackColors(8032420, 15265265));
		ENTITY_COLORS.put(EntityTypes.VILLAGER, new BackpackColors(5651507, 12422002));
		ENTITY_COLORS.put(EntityTypes.VINDICATOR, new BackpackColors(9804699, 2580065));
		ENTITY_COLORS.put(EntityTypes.WANDERING_TRADER, new BackpackColors(4547222, 15377456));
		ENTITY_COLORS.put(EntityTypes.WARDEN, new BackpackColors(1001033, 3790560));
		ENTITY_COLORS.put(EntityTypes.WITCH, new BackpackColors(3407872, 5349438));
		ENTITY_COLORS.put(EntityTypes.WITHER, new BackpackColors(1315860, 5075616));
		ENTITY_COLORS.put(EntityTypes.WITHER_SKELETON, new BackpackColors(1315860, 4672845));
		ENTITY_COLORS.put(EntityTypes.WOLF, new BackpackColors(14144467, 13545366));
		ENTITY_COLORS.put(EntityTypes.ZOGLIN, new BackpackColors(13004373, 15132390));
		ENTITY_COLORS.put(EntityTypes.CREAKING, new BackpackColors(6250335, 16545810));
		ENTITY_COLORS.put(EntityTypes.ZOMBIE, new BackpackColors(44975, 7969893));
		ENTITY_COLORS.put(EntityTypes.ZOMBIE_HORSE, new BackpackColors(3232308, 9945732));
		ENTITY_COLORS.put(EntityTypes.ZOMBIE_VILLAGER, new BackpackColors(5651507, 7969893));
		ENTITY_COLORS.put(EntityTypes.ZOMBIFIED_PIGLIN, new BackpackColors(15373203, 5009705));
	}

	public static BackpackColors getBackpackColors(EntityType<?> entityType) {
		return ENTITY_COLORS.getOrDefault(entityType, new BackpackColors(BackpackWrapper.DEFAULT_MAIN_COLOR, BackpackWrapper.DEFAULT_ACCENT_COLOR));
	}

	public record BackpackColors(int main, int accent) {}
}
