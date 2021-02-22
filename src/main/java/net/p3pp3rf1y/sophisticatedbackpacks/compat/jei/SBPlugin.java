package net.p3pp3rf1y.sophisticatedbackpacks.compat.jei;

import mezz.jei.api.IModPlugin;
import mezz.jei.api.JeiPlugin;
import mezz.jei.api.constants.VanillaRecipeCategoryUid;
import mezz.jei.api.gui.handlers.IGuiContainerHandler;
import mezz.jei.api.helpers.IStackHelper;
import mezz.jei.api.ingredients.subtypes.ISubtypeInterpreter;
import mezz.jei.api.recipe.transfer.IRecipeTransferHandlerHelper;
import mezz.jei.api.registration.IGuiHandlerRegistration;
import mezz.jei.api.registration.IRecipeCatalystRegistration;
import mezz.jei.api.registration.IRecipeRegistration;
import mezz.jei.api.registration.IRecipeTransferRegistration;
import mezz.jei.api.registration.ISubtypeRegistration;
import net.minecraft.client.renderer.Rectangle2d;
import net.minecraft.item.ItemStack;
import net.minecraft.util.ResourceLocation;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;
import net.p3pp3rf1y.sophisticatedbackpacks.api.CapabilityBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.BackpackScreen;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModItems;

import java.util.ArrayList;
import java.util.List;

@SuppressWarnings("unused")
@JeiPlugin
public class SBPlugin implements IModPlugin {
	@Override
	public ResourceLocation getPluginUid() {
		return new ResourceLocation(SophisticatedBackpacks.MOD_ID, "default");
	}

	@Override
	public void registerItemSubtypes(ISubtypeRegistration registration) {
		ISubtypeInterpreter backpackNbtInterpreter = itemStack -> itemStack.getCapability(CapabilityBackpackWrapper.getCapabilityInstance())
				.map(wrapper -> "{clothColor:" + wrapper.getClothColor() + ",borderColor:" + wrapper.getBorderColor() + "}")
				.orElse("");
		registration.registerSubtypeInterpreter(ModItems.BACKPACK.get(), backpackNbtInterpreter);
		registration.registerSubtypeInterpreter(ModItems.IRON_BACKPACK.get(), backpackNbtInterpreter);
		registration.registerSubtypeInterpreter(ModItems.GOLD_BACKPACK.get(), backpackNbtInterpreter);
		registration.registerSubtypeInterpreter(ModItems.DIAMOND_BACKPACK.get(), backpackNbtInterpreter);
		registration.registerSubtypeInterpreter(ModItems.NETHERITE_BACKPACK.get(), backpackNbtInterpreter);
	}

	@Override
	public void registerGuiHandlers(IGuiHandlerRegistration registration) {
		registration.addGuiContainerHandler(BackpackScreen.class, new IGuiContainerHandler<BackpackScreen>() {
			@Override
			public List<Rectangle2d> getGuiExtraAreas(BackpackScreen gui) {
				List<Rectangle2d> ret = new ArrayList<>();
				gui.getUpgradeSlotsRectangle().ifPresent(ret::add);
				ret.addAll(gui.getUpgradeSettingsControl().getTabRectangles());
				gui.getSortButtonsRectangle().ifPresent(ret::add);
				return ret;
			}
		});
	}

	@Override
	public void registerRecipes(IRecipeRegistration registration) {
		registration.addRecipes(DyeRecipesMaker.getRecipes(), VanillaRecipeCategoryUid.CRAFTING);
	}

	@Override
	public void registerRecipeCatalysts(IRecipeCatalystRegistration registration) {
		registration.addRecipeCatalyst(new ItemStack(ModItems.CRAFTING_UPGRADE.get()), VanillaRecipeCategoryUid.CRAFTING);
	}

	@Override
	public void registerRecipeTransferHandlers(IRecipeTransferRegistration registration) {
		IRecipeTransferHandlerHelper handlerHelper = registration.getTransferHelper();
		IStackHelper stackHelper = registration.getJeiHelpers().getStackHelper();
		registration.addRecipeTransferHandler(new CraftingContainerRecipeTransferHandler(handlerHelper, stackHelper), VanillaRecipeCategoryUid.CRAFTING);
	}
}
