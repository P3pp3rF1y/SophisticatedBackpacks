package net.p3pp3rf1y.sophisticatedbackpacks.compat.curios;

import net.minecraft.entity.LivingEntity;
import top.theillusivec4.curios.api.SlotTypePreset;
import top.theillusivec4.curios.api.type.capability.ICurio;

public class CuriosBackpackWrapper implements ICurio {
	@Override
	public boolean canEquip(String identifier, LivingEntity livingEntity) {
		return identifier.equals(SlotTypePreset.BACK.getIdentifier());
	}
}
