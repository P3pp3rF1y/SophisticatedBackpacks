package net.p3pp3rf1y.sophisticatedbackpacks;

import net.minecraft.item.ItemGroup;
import net.minecraftforge.fml.DistExecutor;
import net.minecraftforge.fml.common.Mod;
import net.p3pp3rf1y.sophisticatedbackpacks.client.ClientProxy;
import net.p3pp3rf1y.sophisticatedbackpacks.common.CommonProxy;

@Mod(SophisticatedBackpacks.MOD_ID)
public class SophisticatedBackpacks {
	public static final String MOD_ID = "sophisticatedbackpacks";

	public static final CommonProxy PROXY = DistExecutor.safeRunForDist(() -> ClientProxy::new, () -> CommonProxy::new);
	public static final ItemGroup ITEM_GROUP = new SBItemGroup();

	@SuppressWarnings("java:S1118") //needs to be public for mod to work
	public SophisticatedBackpacks() {
		PROXY.registerHandlers();
	}
}
